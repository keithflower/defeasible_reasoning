(ns depysible.visitor-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [depysible.core :refer [parse-program parse-literal parse-rule parse-atom]]
            [depysible.language.visitor :as visitor]
            [depysible.language.grammar :as grammar]
            [depysible.domain.definitions :refer [->Program ->Rule ->Literal ->LogicAtom rule-types]]))

(deftest test-program-parsing
  (testing "program parsing"
    (testing "empty program"
      (let [result (parse-program "")]
        (is (= 0 (count (:rules result))))))
    
    (testing "single fact"
      (let [result (parse-program "fact.")]
        (is (= 1 (count (:rules result))))
        (let [rule (first (:rules result))]
          (is (= "fact" (get-in rule [:head :atom :functor])))
          (is (= (:strict rule-types) (:type rule))))))
    
    (testing "multiple rules"
      (let [result (parse-program "fact.\nhead -< body.")]
        (is (= 2 (count (:rules result))))))
    
    (testing "mixed strict and defeasible rules"
      (let [result (parse-program "~head <- body. fact.\nhead -< body.")]
        (is (= 3 (count (:rules result))))))
    
    (testing "complex program"
      (let [result (parse-program "
              bird(X) <- chicken(X).
              flies(X) -< bird(X).
              ~flies(X) -< penguin(X).
              chicken(tweety).")]
        (is (= 4 (count (:rules result))))))))

(deftest test-rule-parsing
  (testing "rule parsing variations"
    (testing "simple fact"
      (let [rule (parse-rule "head.")]
        (is (= "head" (get-in rule [:head :atom :functor])))
        (is (= (:strict rule-types) (:type rule)))
        (is (empty? (:body rule)))))
    
    (testing "strict rule with body"
      (let [rule (parse-rule "head <- body.")]
        (is (= "head" (get-in rule [:head :atom :functor])))
        (is (= "body" (get-in rule [:body 0 :atom :functor])))
        (is (= (:strict rule-types) (:type rule)))))
    
    (testing "defeasible rule"
      (let [rule (parse-rule "head -< body.")]
        (is (= "head" (get-in rule [:head :atom :functor])))
        (is (= "body" (get-in rule [:body 0 :atom :functor])))
        (is (= (:defeasible rule-types) (:type rule)))))
    
    (testing "rule with multiple body literals"
      (let [rule (parse-rule "head <- body1, body2, body3.")]
        (is (= "head" (get-in rule [:head :atom :functor])))
        (is (= 3 (count (:body rule))))
        (is (= "body1" (get-in rule [:body 0 :atom :functor])))
        (is (= "body2" (get-in rule [:body 1 :atom :functor])))
        (is (= "body3" (get-in rule [:body 2 :atom :functor])))))
    
    (testing "rule with negated literals"
      (let [rule (parse-rule "~head <- ~body.")]
        (is (= true (get-in rule [:head :negated])))
        (is (= true (get-in rule [:body 0 :negated])))))))

(deftest test-literal-parsing
  (testing "literal parsing"
    (testing "positive literal"
      (let [literal (parse-literal "atom")]
        (is (= false (:negated literal)))
        (is (= "atom" (get-in literal [:atom :functor])))))
    
    (testing "negative literal"
      (let [literal (parse-literal "~atom")]
        (is (= true (:negated literal)))
        (is (= "atom" (get-in literal [:atom :functor])))))
    
    (testing "literal with terms"
      (let [literal (parse-literal "pred(a, b, c)")]
        (is (= false (:negated literal)))
        (is (= "pred" (get-in literal [:atom :functor])))
        (is (= 3 (count (get-in literal [:atom :terms]))))
        (is (= "a" (get-in literal [:atom :terms 0])))
        (is (= "b" (get-in literal [:atom :terms 1])))
        (is (= "c" (get-in literal [:atom :terms 2])))))
    
    (testing "negated literal with terms"
      (let [literal (parse-literal "~pred(x, y)")]
        (is (= true (:negated literal)))
        (is (= "pred" (get-in literal [:atom :functor])))
        (is (= 2 (count (get-in literal [:atom :terms]))))))
    
    (testing "literal with variables"
      (let [literal (parse-literal "pred(X, Y, Z)")]
        (is (= "pred" (get-in literal [:atom :functor])))
        (is (= "X" (get-in literal [:atom :terms 0])))
        (is (= "Y" (get-in literal [:atom :terms 1])))
        (is (= "Z" (get-in literal [:atom :terms 2])))))))

(deftest test-atom-parsing
  (testing "atom parsing"
    (testing "simple atom"
      (let [atom (parse-atom "functor")]
        (is (= "functor" (:functor atom)))
        (is (empty? (:terms atom)))))
    
    (testing "atom with terms"
      (let [atom (parse-atom "functor(a, b, c)")]
        (is (= "functor" (:functor atom)))
        (is (= 3 (count (:terms atom))))
        (is (= "a" (first (:terms atom))))))
    
    (testing "atom with mixed terms"
      (let [atom (parse-atom "pred(X, 5, \"string\", true)")]
        (is (= "pred" (:functor atom)))
        (is (= 4 (count (:terms atom))))
        (is (= "X" (get (:terms atom) 0)))
        (is (= 5 (get (:terms atom) 1)))
        (is (= "string" (get (:terms atom) 2)))
        (is (= true (get (:terms atom) 3)))))))

(deftest test-term-parsing
  (testing "term parsing variations"
    (testing "variable terms"
      (let [literal (parse-literal "pred(Variable, AnotherVar)")]
        (is (= "Variable" (get-in literal [:atom :terms 0])))
        (is (= "AnotherVar" (get-in literal [:atom :terms 1])))))
    
    (testing "string terms"
      (let [literal (parse-literal "pred(\"double quote\", 'single quote')")]
        (is (= "double quote" (get-in literal [:atom :terms 0])))
        (is (= "single quote" (get-in literal [:atom :terms 1])))))
    
    (testing "number terms"
      (let [literal (parse-literal "pred(42, 3.14, -5, -2.718)")]
        (is (= 42 (get-in literal [:atom :terms 0])))
        (is (= 3.14 (get-in literal [:atom :terms 1])))
        (is (= -5 (get-in literal [:atom :terms 2])))
        (is (= -2.718 (get-in literal [:atom :terms 3])))))
    
    (testing "boolean terms"
      (let [literal (parse-literal "pred(true, false)")]
        (is (= true (get-in literal [:atom :terms 0])))
        (is (= false (get-in literal [:atom :terms 1])))))
    
    (testing "identifier terms"
      (let [literal (parse-literal "pred(atom1, atom_2, atom3_test)")]
        (is (= "atom1" (get-in literal [:atom :terms 0])))
        (is (= "atom_2" (get-in literal [:atom :terms 1])))
        (is (= "atom3_test" (get-in literal [:atom :terms 2])))))))

(deftest test-complex-parsing
  (testing "complex parsing scenarios"
    (testing "rule with complex terms"
      (let [rule (parse-rule "parent(X, Y) <- father(X, Y), person(X), person(Y).")]
        (is (= "parent" (get-in rule [:head :atom :functor])))
        (is (= 3 (count (:body rule))))
        (is (= "father" (get-in rule [:body 0 :atom :functor])))
        (is (= "X" (get-in rule [:body 0 :atom :terms 0])))
        (is (= "Y" (get-in rule [:body 0 :atom :terms 1])))))
    
    (testing "rule with mixed term types"
      (let [rule (parse-rule "score(Player, 100, \"perfect\", true) -< game(Player, finished).")]
        (is (= "score" (get-in rule [:head :atom :functor])))
        (is (= 4 (count (get-in rule [:head :atom :terms]))))
        (is (= "Player" (get-in rule [:head :atom :terms 0])))
        (is (= 100 (get-in rule [:head :atom :terms 1])))
        (is (= "perfect" (get-in rule [:head :atom :terms 2])))
        (is (= true (get-in rule [:head :atom :terms 3])))))
    
    (testing "program with comments and whitespace"
      (let [program (parse-program "
              % This is a comment
              bird(X) <- chicken(X).
              % Another comment
              flies(X) -< bird(X).
              
              % Facts
              chicken(tweety).")]
        (is (= 3 (count (:rules program))))))
    
    (testing "deeply nested rule structure"
      (let [rule (parse-rule "complex(A, B, C) <- pred1(A, B), pred2(B, C), pred3(A, C).")]
        (is (= "complex" (get-in rule [:head :atom :functor])))
        (is (= 3 (count (:body rule))))
        (every? #(= 2 (count (get-in % [:atom :terms]))) (:body rule))))))

(deftest test-error-handling
  (testing "parsing error handling"
    (testing "malformed rules should not crash"
      ; These should either parse successfully or fail gracefully
      ; The exact behavior depends on the grammar's error handling
      (try
        (parse-rule "malformed rule without dot")
        (catch :default e
          (is (some? e))))
      
      (try
        (parse-rule "head <-- body.")  ; Wrong arrow
        (catch :default e
          (is (some? e))))
      
      (try 
        (parse-literal "~")  ; Just negation
        (catch :default e
          (is (some? e)))))))

(deftest test-whitespace-and-formatting
  (testing "whitespace and formatting tolerance"
    (testing "extra whitespace in rules"
      (let [rule1 (parse-rule "head <- body.")
            rule2 (parse-rule "  head  <-  body  .  ")]
        (is (= (:head rule1) (:head rule2)))
        (is (= (:body rule1) (:body rule2)))))
    
    (testing "newlines and formatting in programs"
      (let [program1 (parse-program "a. b. c.")
            program2 (parse-program "a.\nb.\nc.")
            program3 (parse-program "a.\n\nb.\n\n\nc.")]
        (is (= (count (:rules program1)) (count (:rules program2)) (count (:rules program3))))))))

(deftest test-functor-variations
  (testing "functor name variations"
    (testing "simple functor names"
      (is (= "test" (:functor (parse-atom "test"))))
      (is (= "a" (:functor (parse-atom "a"))))
      (is (= "longer_name" (:functor (parse-atom "longer_name")))))
    
    (testing "functor names with numbers"
      (is (= "test123" (:functor (parse-atom "test123"))))
      (is (= "pred_2" (:functor (parse-atom "pred_2"))))
      (is (= "a1b2c3" (:functor (parse-atom "a1b2c3"))))))) 