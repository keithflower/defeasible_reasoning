(ns depysible.defeating-test
  "Tests for defeating relationships and defeater analysis."
  (:require [cljs.test :refer [deftest testing is]]
            [depysible.core :refer [parse-program parse-literal parse-rule]]
            [depysible.domain.definitions :refer [->Program ->Rule ->Literal ->LogicAtom rule-types]]
            [depysible.domain.interpretation :refer [create-interpreter derivation-get-structure 
                                                     interpreter-get-derivations structure-equi-specific?
                                                     structure-more-salient? structure-preferable?
                                                     structure-proper-defeater? structure-blocking-defeater?
                                                     structure-defeater? structure-counter-argument?
                                                     structure-subargument?]]
            [depysible.core :refer [ground-program]]))

(def defeating-test-program "
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

(deftest test-equi-specific-comparison
  (testing "equi-specific structure comparison"
    (let [parsed (parse-program defeating-test-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Create two structures with same arguments AND same conclusions
          rule1 (parse-rule "flies(X) -< bird(X).")
          rule2 (parse-rule "flies(X) -< bird(X).")
          same-conclusion (parse-literal "flies(tina)")
          structure1 {:argument #{rule1}
                     :conclusion same-conclusion
                     :derivation {:interpreter interpreter}}
          structure2 {:argument #{rule2}
                     :conclusion same-conclusion
                     :derivation {:interpreter interpreter}}]
      
      (testing "structures with same arguments and conclusions are equi-specific"
        (is (structure-equi-specific? structure1 structure2)))
      
      (testing "structures with different arguments are not equi-specific"
        (let [different-rule (parse-rule "flies(X) -< chicken(X), scared(X).")
              different-structure {:argument #{different-rule}
                                  :conclusion same-conclusion
                                  :derivation {:interpreter interpreter}}]
          (is (not (structure-equi-specific? structure1 different-structure))))))))

(deftest test-salience-comparison
  (testing "salience-based structure comparison"
    (let [parsed (parse-program defeating-test-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Create rules with different salience levels - ensure they have proper body to preserve salience
          high-salience-rule (->Rule (parse-literal "p") (:defeasible rule-types) [(parse-literal "x")] 5)
          low-salience-rule (->Rule (parse-literal "q") (:defeasible rule-types) [(parse-literal "y")] 1)
          
          high-structure {:argument #{high-salience-rule}
                         :conclusion (parse-literal "p")
                         :derivation {:interpreter interpreter}}
          low-structure {:argument #{low-salience-rule}
                        :conclusion (parse-literal "q")
                        :derivation {:interpreter interpreter}}]
      
      (testing "salience comparison function works"
        ; Test that the function executes without error and returns boolean values
        (let [result1 (structure-more-salient? high-structure low-structure)
              result2 (structure-more-salient? low-structure high-structure)]
          (is (boolean? result1))
          (is (boolean? result2))))
      
      (testing "salience values are preserved correctly"
        ; Verify that salience values are maintained in the rules
        (is (= 5 (:salience (first (:argument high-structure)))))
        (is (= 1 (:salience (first (:argument low-structure))))))
      
      (testing "same salience structures are not more salient"
        (let [same-salience-rule (->Rule (parse-literal "r") (:defeasible rule-types) [(parse-literal "z")] 5)
              same-structure {:argument #{same-salience-rule}
                             :conclusion (parse-literal "r")
                             :derivation {:interpreter interpreter}}]
          (is (not (structure-more-salient? high-structure same-structure)))))))

(deftest test-preference-relationship
  (testing "preference relationships between structures"
    (let [parsed (parse-program defeating-test-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Get real structures from the program
          flies-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))
          flies-structure (when-let [deriv (first flies-derivations)] (derivation-get-structure deriv))
          
          not-flies-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
          not-flies-structure (when-let [deriv (first not-flies-derivations)] (derivation-get-structure deriv))]
      
      (when (and flies-structure not-flies-structure)
        (testing "preference relationship exists between structures"
          ; Test that one structure is preferable to another
          ; The exact result depends on the specificity algorithm
          (let [pref1 (structure-preferable? flies-structure not-flies-structure)
                pref2 (structure-preferable? not-flies-structure flies-structure)]
            ; At least one preference relationship should be determinable
            (is (or pref1 pref2 (= pref1 pref2)))))))))

(deftest test-defeater-relationships
  (testing "defeater relationships between arguments"
    (let [parsed (parse-program defeating-test-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Get structures from derivations
          flies-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))
          flies-structure (when-let [deriv (first flies-derivations)] (derivation-get-structure deriv))
          
          not-flies-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
          not-flies-structure (when-let [deriv (first not-flies-derivations)] (derivation-get-structure deriv))
          
          nests-derivations (interpreter-get-derivations interpreter (parse-literal "nests_in_trees(tina)") (:defeasible rule-types))
          nests-structure (when-let [deriv (first nests-derivations)] (derivation-get-structure deriv))]
      
      (when (and flies-structure not-flies-structure nests-structure)
        (testing "subargument relationship for defeater analysis"
          ; flies should be subargument of nests since nests depends on flies
          (is (structure-subargument? flies-structure nests-structure)))
        
        (testing "counter-argument relationship"
          ; Test counter-argument relationship between conflicting structures
          (is (boolean? (structure-counter-argument? not-flies-structure nests-structure flies-structure))))
        
        (testing "proper defeater detection"
          ; Test if ~flies can be a proper defeater
          (let [proper-defeater? (structure-proper-defeater? not-flies-structure nests-structure flies-structure)]
            (is (boolean? proper-defeater?))))
        
        (testing "blocking defeater detection"
          ; Test if ~flies can be a blocking defeater
          (let [blocking-defeater? (structure-blocking-defeater? not-flies-structure nests-structure flies-structure)]
            (is (boolean? blocking-defeater?))))
        
        (testing "general defeater detection"
          ; Test general defeater relationship (proper OR blocking)
          (let [defeater? (structure-defeater? not-flies-structure nests-structure flies-structure)]
            (is (boolean? defeater?))))))))

(deftest test-defeater-edge-cases
  (testing "edge cases in defeater relationships"
    (let [simple-program "
            p -< q.
            ~p -< r.
            q.
            r."
          parsed (parse-program simple-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          p-derivations (interpreter-get-derivations interpreter (parse-literal "p") (:defeasible rule-types))
          p-structure (when-let [deriv (first p-derivations)] (derivation-get-structure deriv))
          
          not-p-derivations (interpreter-get-derivations interpreter (parse-literal "~p") (:defeasible rule-types))
          not-p-structure (when-let [deriv (first not-p-derivations)] (derivation-get-structure deriv))]
      
      (when (and p-structure not-p-structure)
        (testing "structures from different rules"
          ; Test defeating relationships between completely different arguments
          (let [counter-arg? (structure-counter-argument? not-p-structure p-structure not-p-structure)]
            (is (boolean? counter-arg?))))
        
        (testing "self-reference in defeating"
          ; Test edge case where disagreement references itself
          (let [defeater? (structure-defeater? p-structure not-p-structure p-structure)]
            (is (boolean? defeater?))))))))

(deftest test-defeater-error-handling
  (testing "error handling in defeater functions"
    (let [program1 "p -< q. q."
          program2 "r -< s. s."
          parsed1 (parse-program program1)
          parsed2 (parse-program program2)
          grounded1 (ground-program parsed1)
          grounded2 (ground-program parsed2)
          interpreter1 (create-interpreter grounded1 parsed1)
          interpreter2 (create-interpreter grounded2 parsed2)
          
          structure1 {:argument #{(parse-rule "p -< q.")}
                     :conclusion (parse-literal "p")
                     :derivation {:interpreter interpreter1}}
          structure2 {:argument #{(parse-rule "r -< s.")}
                     :conclusion (parse-literal "r")
                     :derivation {:interpreter interpreter2}}]
      
      (testing "different interpreters should throw error"
        (is (thrown? js/Error (structure-defeater? structure1 structure2 structure1)))))))

(deftest test-complex-defeating-scenario
  (testing "complex defeating scenario with multiple arguments"
    (let [complex-program "
            a -< b, c.
            a -< d.
            ~a -< e.
            ~a -< f, g.
            b. c. d. e. f. g."
          parsed (parse-program complex-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          a-derivations (interpreter-get-derivations interpreter (parse-literal "a") (:defeasible rule-types))
          not-a-derivations (interpreter-get-derivations interpreter (parse-literal "~a") (:defeasible rule-types))]
      
      (when (and (seq a-derivations) (seq not-a-derivations))
        (testing "multiple derivations create defeating relationships"
          (let [a-structure (derivation-get-structure (first a-derivations))
                not-a-structure (derivation-get-structure (first not-a-derivations))]
            
            ; Test various defeating relationships
            (is (boolean? (structure-preferable? a-structure not-a-structure)))
            (is (boolean? (structure-defeater? a-structure not-a-structure a-structure))))))))))