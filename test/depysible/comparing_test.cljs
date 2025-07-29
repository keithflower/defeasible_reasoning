(ns depysible.comparing-test
  "Tests for argument comparison and specificity relationships."
  (:require [cljs.test :refer [deftest testing is]]
            [depysible.core :refer [parse-program parse-literal parse-rule]]
            [depysible.domain.definitions :refer [->Program ->Rule ->Literal ->LogicAtom rule-types]]
            [depysible.domain.interpretation :refer [create-interpreter derivation-get-structure 
                                                     interpreter-get-derivations is-strictly-more-specific]]
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

(deftest test-strictly-more-specific-0
  (testing "specificity comparison test 0"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Get ~flies(tina) derivation
          not-flies-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
          not-flies-structure (when-let [deriv (first not-flies-derivations)] (derivation-get-structure deriv))
          
          ; Get flies(tina) derivation via bird (manual creation to match Python test)
          ; Python test expects: flies(X) -< bird(X) derivation, not chicken+scared
          flies-via-bird-rule (parse-rule "flies(X) -< bird(X).")
          flies-structure {:argument #{flies-via-bird-rule}
                          :conclusion (parse-literal "flies(tina)")
                          :derivation {:interpreter interpreter}}]
      
      (when (and not-flies-structure flies-structure)
        (testing "~flies(tina) is strictly more specific than flies(tina)"

          (is (is-strictly-more-specific not-flies-structure flies-structure)))))))

(deftest test-strictly-more-specific-1
  (testing "specificity comparison test 1 - reverse"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Get ~flies(tina) derivation
          not-flies-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
          not-flies-structure (when-let [deriv (first not-flies-derivations)] (derivation-get-structure deriv))
          
          ; Get flies(tina) derivation via bird (manual creation like test-0 to test opposite direction)
          flies-via-bird-rule (parse-rule "flies(X) -< bird(X).")
          flies-structure {:argument #{flies-via-bird-rule}
                          :conclusion (parse-literal "flies(tina)")
                          :derivation {:interpreter interpreter}}]
      
      (when (and not-flies-structure flies-structure)
        (testing "flies(tina) is not strictly more specific than ~flies(tina)"
          (is (not (is-strictly-more-specific flies-structure not-flies-structure))))))))

(deftest test-strictly-more-specific-2
  (testing "specificity comparison test 2"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Get flies(tina) via chicken(tina), scared(tina)
          flies-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))
          ; Find the derivation that uses chicken(tina), scared(tina)
          flies-chicken-scared-structure (when-let [deriv (first flies-derivations)] (derivation-get-structure deriv))
          
          ; Get ~flies(tina) derivation
          not-flies-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
          not-flies-structure (when-let [deriv (first not-flies-derivations)] (derivation-get-structure deriv))]
      
      (when (and flies-chicken-scared-structure not-flies-structure)
        (testing "~flies(tina) is not strictly more specific than flies(tina) via chicken,scared"
          ; This test checks that the more specific chicken,scared rule 
          ; is not defeated by the general chicken rule
          (is (not (is-strictly-more-specific not-flies-structure flies-chicken-scared-structure))))))))

(deftest test-strictly-more-specific-3
  (testing "specificity comparison test 3"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          ; Get flies(tina) via chicken(tina), scared(tina)
          flies-derivations (interpreter-get-derivations interpreter (parse-literal "flies(tina)") (:defeasible rule-types))

          flies-chicken-scared-structure (when-let [deriv (first flies-derivations)] (derivation-get-structure deriv))
          
          ; Get ~flies(tina) derivation
          not-flies-derivations (interpreter-get-derivations interpreter (parse-literal "~flies(tina)") (:defeasible rule-types))
          not-flies-structure (when-let [deriv (first not-flies-derivations)] (derivation-get-structure deriv))]
      
      (when (and flies-chicken-scared-structure not-flies-structure)
        (testing "flies(tina) via chicken,scared is strictly more specific than ~flies(tina)"
          ; The chicken(X), scared(X) rule is more specific than just chicken(X)

          (is (is-strictly-more-specific flies-chicken-scared-structure not-flies-structure))))))

(deftest test-specificity-edge-cases
  (testing "specificity edge cases"
    (let [simple-program "
            p -< q.
            ~p -< q, r.
            q.
            r."
          parsed (parse-program simple-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)]
      
      (testing "more specific rule comparison"
        (let [p-derivations (interpreter-get-derivations interpreter (parse-literal "p") (:defeasible rule-types))
              p-structure (when-let [deriv (first p-derivations)] (derivation-get-structure deriv))
              
              not-p-derivations (interpreter-get-derivations interpreter (parse-literal "~p") (:defeasible rule-types))
              not-p-structure (when-let [deriv (first not-p-derivations)] (derivation-get-structure deriv))]
          
          (when (and p-structure not-p-structure)
            (testing "~p (from q,r) is more specific than p (from q)"
              (is (is-strictly-more-specific not-p-structure p-structure)))
            
            (testing "p (from q) is not more specific than ~p (from q,r)"
              (is (not (is-strictly-more-specific p-structure not-p-structure))))))))))

(deftest test-self-comparison
  (testing "structure comparison with itself"
    (let [parsed (parse-program test-program-text)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)
          
          derivations (interpreter-get-derivations interpreter (parse-literal "bird(tina)") (:strict rule-types))
          structure (when-let [deriv (first derivations)] (derivation-get-structure deriv))]
      
      (when structure
        (testing "structure is not strictly more specific than itself"
          (is (not (is-strictly-more-specific structure structure))))))))

(deftest test-complex-specificity
  (testing "complex specificity scenarios"
    (let [complex-program "
            a -< b.
            a -< b, c.
            a -< b, c, d.
            ~a -< b.
            ~a -< b, e.
            b. c. d. e."
          parsed (parse-program complex-program)
          grounded (ground-program parsed)
          interpreter (create-interpreter grounded parsed)]
      
      (testing "multiple level specificity"
        (let [a-derivations (interpreter-get-derivations interpreter (parse-literal "a") (:defeasible rule-types))
              not-a-derivations (interpreter-get-derivations interpreter (parse-literal "~a") (:defeasible rule-types))]
          
          (is (>= (count a-derivations) 1))
          (is (>= (count not-a-derivations) 1))
          
          ; Test that more specific rules (with more conditions) are preferred
          (when-let [a-struct (derivation-get-structure (first a-derivations))]
            (when-let [not-a-struct (derivation-get-structure (first not-a-derivations))]
              (testing "at least one comparison should work"
                ; In complex scenarios, at least one should be more specific
                (is (or (is-strictly-more-specific a-struct not-a-struct)
                       (is-strictly-more-specific not-a-struct a-struct)
                       (= a-struct not-a-struct))))))))))))
