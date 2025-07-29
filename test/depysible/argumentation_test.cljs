(ns depysible.argumentation-test
  "Tests for argumentation structures and subargument relationships."
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [depysible.core :refer [parse-program parse-literal parse-rule]]
            [depysible.domain.definitions :refer [->Program ->Rule ->Literal ->LogicAtom rule-types]]
            [depysible.domain.interpretation :refer [create-interpreter derivation-get-structure create-derivation
                                                     interpreter-get-derivations structure-strict? structure-subargument?
                                                     query-with-full-explanation]]
            [depysible.core :refer [ground-program]]))

(def test-program-text "
    bird(X) <- chicken(X).
    bird(X) <- penguin(X).
    ~flies(X) <- penguin(X).
    chicken(tina).
    penguin(tweety).
    scared(tina).
    flies(X) -< bird(X).
    flies(X) -< chicken(X), scared(X).
    nests_in_trees(X) -< flies(X).
    ~flies(X) -< chicken(X).")

(deftest test-argument-structure-creation
  (testing "get structure for strict argument"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)
          derivations (interpreter-get-derivations interpreter (parse-literal "bird(tina)") (:strict rule-types))]
      
      (when-let [derivation (first derivations)]
        (let [structure (derivation-get-structure derivation)]
          (testing "structure properties"
            (is (some? structure))
            (is (= (parse-literal "bird(tina)") (:conclusion structure)))
            (is (structure-strict? structure)))))))

(deftest test-argument-structure-defeasible
  (testing "get structure for defeasible argument"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)
          derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))]
      
      (when-let [derivation (first derivations)]
        (let [structure (derivation-get-structure derivation)]
          (testing "defeasible structure properties"
            (is (some? structure))
            (is (= (parse-literal "~flies(tina)") (:conclusion structure)))
            (is (not (structure-strict? structure)))))))))

(deftest test-flies-arguments
  (testing "flies arguments with different derivations"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)
          flies-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))]
      
      (testing "multiple derivations for flies(tina)"
        (is (>= (count flies-derivations) 1)))
      
      (when-let [derivation (first flies-derivations)]
        (let [structure (derivation-get-structure derivation)]
          (testing "flies structure properties"
            (is (some? structure))
            (is (= (parse-literal "flies(tina)") (:conclusion structure)))
            (is (not (structure-strict? structure)))))))))

(deftest test-subargument-relationships
  (testing "subargument relationships"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)
          
          ; Get bird(tina) derivation (subargument)
          bird-derivations (interpreter-get-derivations interpreter (parse-literal "bird(tina)") (:strict rule-types))
          bird-structure (when-let [deriv (first bird-derivations)] (derivation-get-structure deriv))
          
          ; Get flies(tina) derivation (containing argument)
          flies-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))
          flies-structure (when-let [deriv (first flies-derivations)] (derivation-get-structure deriv))]
      
      (when (and bird-structure flies-structure)
        (testing "bird(tina) is subargument of flies(tina)"
          (is (structure-subargument? bird-structure flies-structure)))
        
        (testing "flies(tina) is not subargument of bird(tina)"
          (is (not (structure-subargument? flies-structure bird-structure))))
        
        (testing "self-subargument relationships"
          (is (structure-subargument? bird-structure bird-structure))
          (is (structure-subargument? flies-structure flies-structure)))))))

(deftest test-complex-argument-scenarios
  (testing "complex argument creation and relationships"
    (let [complex-program "
            a <- b.
            b <- c.
            c.
            d -< a.
            e -< d, f.
            f."
          parsed (parse-program complex-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "strict chain derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "a") (:strict rule-types))]
          (is (= 1 (count derivations)))
          (when-let [structure (derivation-get-structure (first derivations))]
            (is (structure-strict? structure)))))
      
              (testing "defeasible derivation"
          (let [derivations (interpreter-get-derivations interpreter (parse-literal "d") (:defeasible rule-types))]
            (is (>= (count derivations) 1))
            (when-let [structure (derivation-get-structure (first derivations))]
              (is (not (structure-strict? structure))))))
      
              (testing "mixed derivation"
          (let [derivations (interpreter-get-derivations interpreter (parse-literal "e") (:defeasible rule-types))]
            (is (>= (count derivations) 1))
            (when-let [structure (derivation-get-structure (first derivations))]
              (is (not (structure-strict? structure)))))))))

(deftest test-argument-creation-edge-cases
  (testing "argument creation edge cases"
    (testing "empty program"
      (let [empty-program (parse-program "")
            grounded (ground-program empty-program)
            interpreter (create-interpreter grounded)
            derivations (interpreter-get-derivations interpreter (parse-literal "nonexistent") (:strict rule-types))]
        (is (= 0 (count derivations)))))
    
    (testing "single fact"
      (let [fact-program (parse-program "fact.")
            grounded (ground-program fact-program)
            interpreter (create-interpreter grounded)
            derivations (interpreter-get-derivations interpreter (parse-literal "fact") (:strict rule-types))]
        (is (= 1 (count derivations)))
        (when-let [structure (derivation-get-structure (first derivations))]
          (is (structure-strict? structure)))))))

(deftest test-contradiction-resolution
  (testing "contradiction resolution with dialectical argumentation"
    (let [;; Basic flying example with contradiction
          contradiction-program "
            % Facts
            bird(tina).
            chicken(tina).
            scared(tina).
            
            % General rule: birds fly (low priority)
            flies(X) -< bird(X) [1].
            
            % More specific rule: chickens don't fly (medium priority)
            ~flies(X) -< chicken(X) [2].
            
            % Exception: scared chickens do fly (high priority)
            flies(X) -< chicken(X), scared(X) [3]."
          parsed (parse-program contradiction-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "positive literal should win with higher salience"
        ;; Use full dialectical query to test contradiction resolution
        (let [result (query-with-full-explanation interpreter (parse-literal "flies(tina)"))]
          (is (= "YES" (:answer result)) "flies(tina) should be YES due to higher salience rule")))
      
      (testing "negative literal should be defeated"
        ;; Use full dialectical query to test contradiction resolution  
        (let [result (query-with-full-explanation interpreter (parse-literal "~flies(tina)"))]
          (is (= "NO" (:answer result)) "~flies(tina) should be NO as it's defeated by more specific argument")))
      
      (testing "facts should still work correctly"
        (let [bird-result (query-with-full-explanation interpreter (parse-literal "bird(tina)"))
              chicken-result (query-with-full-explanation interpreter (parse-literal "chicken(tina)"))
              scared-result (query-with-full-explanation interpreter (parse-literal "scared(tina)"))]
          (is (= "YES" (:answer bird-result)) "bird(tina) should be YES")
          (is (= "YES" (:answer chicken-result)) "chicken(tina) should be YES") 
          (is (= "YES" (:answer scared-result)) "scared(tina) should be YES")))
      
      (testing "verify no double contradiction"
        ;; Both cannot be YES - this would be a logical error
        (let [pos-result (query-with-full-explanation interpreter (parse-literal "flies(tina)"))
              neg-result (query-with-full-explanation interpreter (parse-literal "~flies(tina)"))]
          (is (not (and (= "YES" (:answer pos-result)) 
                       (= "YES" (:answer neg-result))))
              "Both flies(tina) and ~flies(tina) cannot be YES - this would be a contradiction"))))))
)
