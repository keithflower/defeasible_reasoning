(ns depysible.language-test
  "Tests for language parsing and derivation functionality."
  (:require [cljs.test :refer [deftest testing is]]
            [depysible.core :refer [parse-program parse-literal parse-rule]]
            [depysible.domain.definitions :refer [->Program ->Rule ->Literal ->LogicAtom rule-types rule->string]]
            [depysible.domain.interpretation :refer [create-interpreter interpreter-get-derivations interpreter-contradictory?]]
            [depysible.core :refer [ground-program]]))

(deftest test-get-derivations-simple
  (testing "get derivations for simple facts"
    (let [program-text "
            a <- b, c. 
            b <- d, e. 
            b <- c. 
            c <- f, g. 
            d <- h. 
            e. 
            f. 
            g. 
            h."
          parsed (parse-program program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "fact derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "e") (:strict rule-types))]
          (is (= 1 (count derivations)))))
      
      (testing "single step derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "d") (:strict rule-types))]
          (is (= 1 (count derivations)))))
      
      (testing "multi-step derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "c") (:strict rule-types))]
          (is (= 1 (count derivations)))))
      
      (testing "multiple derivation paths"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "b") (:strict rule-types))]
          (is (= 2 (count derivations)))))
      
      (testing "complex derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "a") (:strict rule-types))]
          (is (= 2 (count derivations))))))))

(deftest test-get-derivations-defeasible
  (testing "get derivations for defeasible rules"
    (let [program-text "
            bird(X) <- chicken(X).
            bird(X) <- penguin(X).
            ~flies(X) <- penguin(X).
            chicken(tina).
            penguin(tweety).
            scared(tina).
            flies(X) -< bird(X).
            flies(X) -< chicken(X), scared(X).
            nests_in_trees(X) -< flies(X).
            ~flies(X) -< chicken(X)."
          parsed (parse-program program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "strict bird derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "bird(tina)") (:strict rule-types))]
          (is (= 1 (count derivations)))))
      
      (testing "defeasible flies derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))]
          (is (>= (count derivations) 1))))
      
      (testing "strict flies derivation should be empty"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:strict rule-types))]
          (is (= 0 (count derivations)))))
      
      (testing "no derivation for negated bird"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "~bird(tina)") (:defeasible rule-types))]
          (is (= 0 (count derivations)))))
      
      (testing "defeasible not-flies derivation"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))]
          (is (>= (count derivations) 1)))))))

(deftest test-contradiction-checking
  (testing "contradiction detection"
    (let [program-text "
            p <- q.
            ~p <- r.
            q.
            r."
          parsed (parse-program program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "strict contradiction"
        (is (interpreter-contradictory? interpreter (:strict rule-types))))
      
      (testing "defeasible rules without contradiction"
        (let [defeasible-program-text "
                p -< q.
                ~p -< r.
                q.
                r."
              parsed-def (parse-program defeasible-program-text)
              grounded-def (ground-program parsed-def)
              interpreter-def (create-interpreter grounded-def)]
          (is (not (interpreter-contradictory? interpreter-def (:defeasible rule-types)))))))))

(deftest test-complex-scenarios
  (testing "complex reasoning scenarios"
    (let [program-text "
            bird(X) <- chicken(X).
            flies(X) -< bird(X).
            ~flies(X) -< penguin(X).
            chicken(tweety).
            penguin(opus)."
          parsed (parse-program program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "tweety flies defeasibly"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "flies(tweety)") (:defeasible rule-types))]
          (is (>= (count derivations) 1))))
      
      (testing "opus doesn't fly"
        (let [derivations (interpreter-get-derivations interpreter (parse-literal "~flies(opus)") (:defeasible rule-types))]
          (is (>= (count derivations) 1)))))))

(deftest test-parsing-integration
  (testing "parsing and language integration"
    (testing "rule parsing"
      (let [rule (parse-rule "flies(X) -< bird(X).")]
        (is (= "flies" (get-in rule [:head :atom :functor])))
        (is (= (:defeasible rule-types) (:type rule)))))
    
    (testing "literal parsing"
      (let [literal (parse-literal "~flies(tweety)")]
        (is (= true (:negated literal)))
        (is (= "flies" (get-in literal [:atom :functor])))))
    
    (testing "program parsing"
      (let [program (parse-program "fact. rule(X) <- fact.")]
        (is (= 2 (count (:rules program)))))))) 