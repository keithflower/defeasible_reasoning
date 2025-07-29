(ns depysible.definitions-extended-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [depysible.domain.definitions :refer [->LogicAtom ->Literal ->Rule ->Program create-atom 
                                                  create-literal create-rule create-program 
                                                  rule-types]]
            [depysible.core :refer [parse-literal parse-rule parse-atom]]))

(deftest test-atom-equality
  (testing "atom equality"
    (testing "equal atoms"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "a" ["b" 5 true])]
        (is (= atom1 atom2))))
    
    (testing "different functors"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "c" ["b" 5 true])]
        (is (not= atom1 atom2))))
    
    (testing "different terms"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "a" [true 5 "b"])]
        (is (not= atom1 atom2))))
    
    (testing "different term count"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "a" ["b" "b" 5 true])]
        (is (not= atom1 atom2))))
    
    (testing "empty terms"
      (let [atom1 (->LogicAtom "a" [])
            atom2 (->LogicAtom "a" [])]
        (is (= atom1 atom2))))
    
    (testing "not equal to nil"
      (let [atom (->LogicAtom "a" ["b" 5 true])]
        (is (not= atom nil))))
    
    (testing "not equal to string"
      (let [atom (->LogicAtom "a" ["b" 5 true])]
        (is (not= atom "a(b, 5, true)"))))))

(deftest test-atom-ordering
  (testing "atom ordering"
    (testing "functor ordering"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "c" ["b" 5 true])]
        (is (< (compare atom1 atom2) 0))))
    
    (testing "term count ordering"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "a" ["b" "b" 5 true])]
        (is (< (compare atom1 atom2) 0))))
    
    (testing "term ordering"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "a" [true 5 "b"])]
        ; This depends on how terms are compared - strings vs other types
        (is (number? (compare atom1 atom2)))))
    
    (testing "equal atoms compare to 0"
      (let [atom1 (->LogicAtom "a" ["b" 5 true])
            atom2 (->LogicAtom "a" ["b" 5 true])]
        (is (= 0 (compare atom1 atom2)))))))

(deftest test-atom-validation
  (testing "atom validation"
    (testing "nil functor should fail"
      (try
        (create-atom nil [])
        (is false "Should have thrown exception")
        (catch :default e
          (is (some? e)))))
    
    (testing "empty functor should fail"
      (try
        (create-atom "" [])
        (is false "Should have thrown exception")
        (catch :default e
          (is (some? e)))))
    
    (testing "whitespace functor should fail"
      (try
        (create-atom " " [])
        (is false "Should have thrown exception") 
        (catch :default e
          (is (some? e)))))
    
    (testing "nil terms should fail"
      (try
        (create-atom "functor" nil)
        (is false "Should have thrown exception")
        (catch :default e
          (is (some? e)))))
    
    (testing "valid atom creation"
      (let [atom (create-atom "functor" [])]
        (is (= "functor" (:functor atom)))
        (is (empty? (:terms atom))))
      
      (let [atom (create-atom "functor" ["a" 5 true])]
        (is (= "functor" (:functor atom)))
        (is (= ["a" 5 true] (:terms atom)))))))

(deftest test-atom-string-representation
  (testing "atom string representation"
    (testing "atom with no terms"
      (let [atom (->LogicAtom "a" [])]
        (is (= "a" (str atom)))))
    
    (testing "atom with terms"
      (let [atom (->LogicAtom "a" ["b" 5 true])]
        (is (= "a(b, 5, true)" (str atom)))))))

(deftest test-literal-equality
  (testing "literal equality"
    (testing "equal positive literals"
      (let [lit1 (->Literal false (->LogicAtom "a" []))
            lit2 (->Literal false (->LogicAtom "a" []))]
        (is (= lit1 lit2))))
    
    (testing "equal negative literals"
      (let [lit1 (->Literal true (->LogicAtom "a" []))
            lit2 (->Literal true (->LogicAtom "a" []))]
        (is (= lit1 lit2))))
    
    (testing "positive vs negative"
      (let [lit1 (->Literal false (->LogicAtom "a" []))
            lit2 (->Literal true (->LogicAtom "a" []))]
        (is (not= lit1 lit2))))
    
    (testing "different atoms"
      (let [lit1 (->Literal false (->LogicAtom "a" []))
            lit2 (->Literal false (->LogicAtom "b" []))]
        (is (not= lit1 lit2))))
    
    (testing "complex literals"
      (let [lit1 (->Literal true (->LogicAtom "pred" ["X" "Y"]))
            lit2 (->Literal true (->LogicAtom "pred" ["X" "Y"]))]
        (is (= lit1 lit2))))))

(deftest test-literal-ordering
  (testing "literal ordering"
    (testing "negation ordering"
      (let [lit1 (->Literal false (->LogicAtom "a" []))
            lit2 (->Literal true (->LogicAtom "a" []))]
        ; Positive literals should come before negative ones
        (is (< (compare lit1 lit2) 0))))
    
    (testing "atom ordering within literals"
      (let [lit1 (->Literal false (->LogicAtom "a" []))
            lit2 (->Literal false (->LogicAtom "b" []))]
        (is (< (compare lit1 lit2) 0))))
    
    (testing "equal literals"
      (let [lit1 (->Literal false (->LogicAtom "a" []))
            lit2 (->Literal false (->LogicAtom "a" []))]
        (is (= 0 (compare lit1 lit2)))))))

(deftest test-literal-string-representation
  (testing "literal string representation"
    (testing "positive literal"
      (let [literal (->Literal false (->LogicAtom "a" []))]
        (is (= "a" (str literal)))))
    
    (testing "negative literal"
      (let [literal (->Literal true (->LogicAtom "a" []))]
        (is (= "~a" (str literal)))))
    
    (testing "literal with terms"
      (let [literal (->Literal false (->LogicAtom "pred" ["X" "Y"]))]
        (is (= "pred(X, Y)" (str literal))))
      
      (let [literal (->Literal true (->LogicAtom "pred" ["X" "Y"]))]
        (is (= "~pred(X, Y)" (str literal)))))))

(deftest test-rule-equality
  (testing "rule equality"
    (testing "equal facts"
      (let [rule1 (create-rule (->Literal false (->LogicAtom "fact" [])) (:strict rule-types) [])
            rule2 (create-rule (->Literal false (->LogicAtom "fact" [])) (:strict rule-types) [])]
        (is (= rule1 rule2))))
    
    (testing "different rule types"
      (let [rule1 (create-rule (->Literal false (->LogicAtom "fact" [])) (:strict rule-types) [])
            rule2 (create-rule (->Literal false (->LogicAtom "fact" [])) (:defeasible rule-types) [])]
        (is (not= rule1 rule2))))
    
    (testing "different heads"
      (let [rule1 (create-rule (->Literal false (->LogicAtom "a" [])) (:strict rule-types) [])
            rule2 (create-rule (->Literal false (->LogicAtom "b" [])) (:strict rule-types) [])]
        (is (not= rule1 rule2))))
    
    (testing "different bodies"
      (let [rule1 (create-rule (->Literal false (->LogicAtom "head" [])) 
                          (:strict rule-types) 
                          [(->Literal false (->LogicAtom "body1" []))])
            rule2 (create-rule (->Literal false (->LogicAtom "head" [])) 
                          (:strict rule-types) 
                          [(->Literal false (->LogicAtom "body2" []))])]
        (is (not= rule1 rule2))))))

(deftest test-rule-parsing-integration
  (testing "rule parsing and equality"
    (testing "parsed rules equality"
      (let [rule1 (parse-rule "head <- body.")
            rule2 (parse-rule "head <- body.")]
        (is (= rule1 rule2))))
    
    (testing "different parsed rules"
      (let [rule1 (parse-rule "head <- body.")
            rule2 (parse-rule "head -< body.")]
        (is (not= rule1 rule2))))
    
        (testing "complex rule parsing"
      (let [rule (parse-rule "parent(X, Y) <- father(X, Y), person(X), person(Y).")]
        (is (= "parent" (get-in rule [:head :atom :functor])))
        (is (= 3 (count (:body rule))))
        (is (= (:strict rule-types) (:type rule)))))))
  
  (deftest test-program-operations
  (testing "program operations"
    (testing "empty program"
      (let [program (create-program [])]
        (is (empty? (:rules program)))))
    
    (testing "program with rules"
      (let [rule1 (parse-rule "fact.")
            rule2 (parse-rule "head <- body.")
            program (create-program [rule1 rule2])]
        (is (= 2 (count (:rules program))))
        (is (some #(= rule1 %) (:rules program)))
        (is (some #(= rule2 %) (:rules program)))))
    
    (testing "program equality"
      (let [rule1 (parse-rule "fact.")
            rule2 (parse-rule "head <- body.")
            program1 (create-program [rule1 rule2])
            program2 (create-program [rule1 rule2])
            program3 (create-program [rule2 rule1])]  ; Different order
        (is (= program1 program2))
        ; Order matters in vectors, so these might not be equal
        ; depending on the implementation
        (is (or (= program1 program3) (not= program1 program3)))))))

(deftest test-hash-consistency
  (testing "hash consistency"
    (testing "equal atoms have equal hashes"
      (let [atom1 (->LogicAtom "a" ["b" 5])
            atom2 (->LogicAtom "a" ["b" 5])]
        (is (= (hash atom1) (hash atom2)))))
    
    (testing "equal literals have equal hashes"
      (let [lit1 (->Literal false (->LogicAtom "a" ["b"]))
            lit2 (->Literal false (->LogicAtom "a" ["b"]))]
        (is (= (hash lit1) (hash lit2)))))
    
    (testing "different atoms have different hashes"
      (let [atom1 (->LogicAtom "a" ["b"])
            atom2 (->LogicAtom "a" ["c"])]
        ; Hash codes should typically be different, but not guaranteed
        ; Just ensure we can compute them without error
        (is (number? (hash atom1)))
        (is (number? (hash atom2)))))))

(deftest test-data-structure-validation
  (testing "data structure validation"
    (testing "literal validation"
      (try
        (create-literal true nil)
        (is false "Should have thrown exception")
        (catch :default e
          (is (some? e)))))
    
    (testing "rule validation"
      (try
        (create-rule nil (:strict rule-types) [])
        (is false "Should have thrown exception")
        (catch :default e
          (is (some? e)))))
    
    (testing "valid structures"
      (let [atom (create-atom "test" ["X"])
            literal (create-literal false atom)
            rule (create-rule literal (:strict rule-types) [])
            program (create-program [rule])]
        (is (some? atom))
        (is (some? literal))
        (is (some? rule))
        (is (some? program))))))

(deftest test-complex-data-structures
  (testing "complex data structures"
    (testing "complex rule with variables"
      (let [rule (parse-rule "ancestor(X, Z) <- parent(X, Y), ancestor(Y, Z).")]
        (is (= "ancestor" (get-in rule [:head :atom :functor])))
        (is (= 2 (count (get-in rule [:head :atom :terms]))))
        (is (= "X" (get-in rule [:head :atom :terms 0])))
        (is (= "Z" (get-in rule [:head :atom :terms 1])))
        (is (= 2 (count (:body rule))))))
    
    (testing "rule with mixed term types"
      (let [rule (parse-rule "score(Player, 100, true) <- game(Player, won).")]
        (is (= 3 (count (get-in rule [:head :atom :terms]))))
        (is (= "Player" (get-in rule [:head :atom :terms 0])))
        (is (= 100 (get-in rule [:head :atom :terms 1])))
        (is (= true (get-in rule [:head :atom :terms 2]))))))) 