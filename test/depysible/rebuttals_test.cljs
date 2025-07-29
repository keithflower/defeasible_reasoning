(ns depysible.rebuttals-test
  "Tests for counter-arguments and rebuttal relationships."
  (:require [cljs.test :refer [deftest testing is]]
            [depysible.core :refer [parse-program parse-literal parse-rule]]
            [depysible.domain.definitions :refer [->Program ->Rule ->Literal ->LogicAtom rule-types]]
            [depysible.domain.interpretation :refer [create-interpreter derivation-get-structure 
                                                     interpreter-get-derivations literals-disagree? is-counter-argument
                                                     structure-subargument?]]
            [depysible.core :refer [ground-program]]))

(deftest test-disagreement-basic
  (testing "basic literal disagreement"
    (testing "direct negation disagreement"
      (let [literal1 (parse-literal "a")
            literal2 (parse-literal "~a")]
        (is (literals-disagree? literal1 literal2 #{}))))
    
    (testing "no disagreement between unrelated literals"
      (let [literal1 (parse-literal "a")
            literal2 (parse-literal "b")]
        (is (not (literals-disagree? literal1 literal2 #{})))))
    
    (testing "indirect disagreement through rules"
      (let [literal1 (parse-literal "a")
            literal2 (parse-literal "b")
            program (parse-program "~h <- b. h <- a.")
            rules (set (:rules program))]
        (is (literals-disagree? literal1 literal2 rules))))))

(deftest test-counter-argument-basic
  (testing "basic counter argument relationships"
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
          interpreter (create-interpreter grounded parsed)]
      
      (testing "no counter argument between unrelated arguments"
        (let [not-flies-tweety-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tweety)") (:strict rule-types))
              not-flies-tweety-structure (when-let [deriv (first not-flies-tweety-derivations)] (derivation-get-structure deriv))
              
              flies-tina-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))
              flies-tina-structure (when-let [deriv (first flies-tina-derivations)] (derivation-get-structure deriv))]
          
          (when (and not-flies-tweety-structure flies-tina-structure)
            (is (not (is-counter-argument not-flies-tweety-structure flies-tina-structure flies-tina-structure)))
            (is (not (is-counter-argument flies-tina-structure not-flies-tweety-structure not-flies-tweety-structure)))))))))

(deftest test-counter-argument-direct
  (testing "direct counter arguments"
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
          interpreter (create-interpreter grounded parsed)]
      
      (testing "counter arguments between contradictory conclusions"
        (let [not-flies-tina-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
              not-flies-tina-structure (when-let [deriv (first not-flies-tina-derivations)] (derivation-get-structure deriv))
              
              flies-tina-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))
              flies-tina-structure (when-let [deriv (first flies-tina-derivations)] (derivation-get-structure deriv))]
          
          (when (and not-flies-tina-structure flies-tina-structure)
            (testing "~flies(tina) is counter argument of flies(tina)"
              (is (is-counter-argument not-flies-tina-structure flies-tina-structure flies-tina-structure)))
            
            (testing "flies(tina) is counter argument of ~flies(tina)"
              (is (is-counter-argument flies-tina-structure not-flies-tina-structure not-flies-tina-structure)))))))))

(deftest test-counter-argument-subargument
  (testing "counter arguments attacking subarguments"
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
          interpreter (create-interpreter grounded parsed)]
      
      (testing "attacking subarguments"
        (let [not-flies-tina-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
              not-flies-tina-structure (when-let [deriv (first not-flies-tina-derivations)] (derivation-get-structure deriv))
              
              nests-tina-derivations (interpreter-get-derivations interpreter (parse-literal "nests_in_trees(tina)") (:defeasible rule-types))
              nests-tina-structure (when-let [deriv (first nests-tina-derivations)] (derivation-get-structure deriv))
              
              flies-tina-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))
              flies-tina-structure (when-let [deriv (first flies-tina-derivations)] (derivation-get-structure deriv))]
          
          (when (and not-flies-tina-structure nests-tina-structure flies-tina-structure)
            (testing "~flies(tina) attacks subargument of nests_in_trees(tina)"
              (let [result (is-counter-argument not-flies-tina-structure nests-tina-structure flies-tina-structure)]
                (is result)))
            
            (testing "reverse attack should not work"
              (is (not (is-counter-argument not-flies-tina-structure flies-tina-structure nests-tina-structure)))))))))

(deftest test-complex-counter-arguments
  (testing "complex counter argument scenarios"
    (let [complex-program "
            p -< q.
            ~p -< r.
            q.
            r.
            s -< p.
            ~s -< ~p."
          parsed (parse-program complex-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "chain of counter arguments"
        (let [p-derivations (interpreter-get-derivations interpreter (parse-literal "p") (:defeasible rule-types))
              p-structure (when-let [deriv (first p-derivations)] (derivation-get-structure deriv))
              
              not-p-derivations (interpreter-get-derivations interpreter (parse-literal "~p") (:defeasible rule-types))
              not-p-structure (when-let [deriv (first not-p-derivations)] (derivation-get-structure deriv))]
          
          (when (and p-structure not-p-structure)
            (testing "p and ~p are counter arguments"
              (is (is-counter-argument p-structure not-p-structure not-p-structure))
              (is (is-counter-argument not-p-structure p-structure p-structure)))))))))

(deftest test-disagreement-edge-cases
  (testing "disagreement edge cases"
    (testing "same literal doesn't disagree with itself"
      (let [literal (parse-literal "a")]
        (is (not (literals-disagree? literal literal #{})))))
    
    (testing "complex rule-based disagreement"
      (let [literal1 (parse-literal "happy(john)")
            literal2 (parse-literal "sad(john)")
            program (parse-program "~happy(X) <- sad(X). happy(john) <- excited(john).")
            rules (set (:rules program))]
        (is (literals-disagree? literal1 literal2 rules))))
    
    (testing "no disagreement without connecting rules"
      (let [literal1 (parse-literal "happy(john)")
            literal2 (parse-literal "sad(mary)")
            program (parse-program "~happy(X) <- sad(X).")
            rules (set (:rules program))]
        (is (not (literals-disagree? literal1 literal2 rules)))))))

(deftest test-counter-argument-edge-cases
  (testing "counter argument edge cases"
    (let [simple-program "fact."
          parsed (parse-program simple-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded)]
      
      (testing "single fact has no counter arguments"
        (let [fact-derivations (interpreter-get-derivations interpreter (parse-literal "fact") (:strict rule-types))
              fact-structure (when-let [deriv (first fact-derivations)] (derivation-get-structure deriv))]
          
          (when fact-structure
            (testing "fact doesn't counter itself"
              (is (not (is-counter-argument fact-structure fact-structure fact-structure)))))))
      
      (testing "empty program"
        (let [empty-program (parse-program "")
              grounded-empty (ground-program empty-program)
              interpreter-empty (create-interpreter grounded-empty)
              derivations (interpreter-get-derivations interpreter-empty (parse-literal "nonexistent") (:strict rule-types))]
          (is (= 0 (count derivations))))))))

(deftest test-disagreement-with-variables
  (testing "disagreement with variables and unification"
    (let [literal1 (parse-literal "flies(tweety)")
          literal2 (parse-literal "penguin(tweety)")
          program (parse-program "~flies(X) <- penguin(X).")
          rules (set (:rules program))]
       
      (testing "variables unify to create disagreement"
        (is (literals-disagree? literal1 literal2 rules)))))) )
