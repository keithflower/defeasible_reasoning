(ns depysible.definitions-test
  "Translation of src/test/python/_tests_definitions.py
   
   This test namespace verifies the correct functionality of the core data structures
   in the definitions namespace. Each test corresponds to a specific test method
   in the Python TestAtom, TestLiteral, and TestRule classes.
   
   Uses cljs.test instead of Python's unittest framework."
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [depysible.domain.definitions :as defs]
            [depysible.core :as core]))

;; ============================================================================
;; Test Atom functionality - corresponds to Python TestAtom class
;; ============================================================================

(deftest test-atom-equality
  "Test Atom equality functionality.
   Corresponds to Python TestAtom.test__atom___eq___* methods (lines 12-40)."
  
  (testing "Atom equality with nil"
    ;; Corresponds to test__atom___eq___0
    (is (not= (defs/create-atom "a" ["b" 5 true]) nil)))
  
  (testing "Atom equality with string"
    ;; Corresponds to test__atom___eq___1  
    (is (not= (defs/create-atom "a" ["b" 5 true]) "a(b, 5, true)")))
  
  (testing "Atom equality with different arity"
    ;; Corresponds to test__atom___eq___2
    (is (not= (defs/create-atom "a" ["b" 5 true])
              (defs/create-atom "a" []))))
  
  (testing "Atom equality with different term order"
    ;; Corresponds to test__atom___eq___3
    (is (not= (defs/create-atom "a" ["b" 5 true])
              (defs/create-atom "a" [true 5 "b"]))))
  
  (testing "Atom equality with different functor"
    ;; Corresponds to test__atom___eq___4
    (is (not= (defs/create-atom "a" ["b" 5 true])
              (defs/create-atom "c" ["b" 5 true]))))
  
  (testing "Atom equality with different term count"
    ;; Corresponds to test__atom___eq___5
    (is (not= (defs/create-atom "a" ["b" 5 true])
              (defs/create-atom "a" ["b" "b" 5 true]))))
  
  (testing "Atom equality - identical atoms"
    ;; Corresponds to test__atom___eq___6
    (is (= (defs/create-atom "a" ["b" 5 true])
           (defs/create-atom "a" ["b" 5 true])))))

(deftest test-atom-hash
  "Test Atom hash functionality.
   Corresponds to Python TestAtom.test__atom___hash___00 method (lines 42-44)."
  
  (testing "Atom hash is not nil"
    (let [atom (defs/create-atom "a" ["b" 5 true])]
      (is (not (nil? (hash atom)))))))

(deftest test-atom-creation
  "Test Atom creation and basic properties.
   Corresponds to Python TestAtom.test__atom___init___* methods (lines 46-75)."
  
  (testing "Create atom with empty terms"
    ;; Corresponds to test__atom___init___03
    (let [atom (defs/create-atom "a" [])]
      (is (= (:functor atom) "a"))
      (is (empty? (:terms atom)))))
  
  (testing "Create atom with terms"
    ;; Corresponds to test__atom___init___04
    (let [atom (defs/create-atom "a" ["b" 5 true])]
      (is (= (:functor atom) "a"))
      (is (= (:terms atom) ["b" 5 true])))))

(deftest test-atom-string-representation
  "Test Atom string representation.
   Corresponds to Python TestAtom.test__atom___repr___* methods (lines 188-196)."
  
  (testing "Atom with no terms"
    ;; Corresponds to test__atom___repr___0
    (let [atom (defs/create-atom "a" [])]
      (is (= (str atom) "a"))))
  
  (testing "Atom with terms"
    ;; Corresponds to test__atom___repr___1
    (let [atom (defs/create-atom "a" ["b" 5 true])]
      (is (= (str atom) "a(b, 5, true)")))))

(deftest test-atom-arity
  "Test Atom arity functionality."
  
  (testing "Empty atom arity"
    (let [atom (defs/create-atom "a" [])]
      (is (= (defs/atom-arity atom) 0))))
  
  (testing "Atom with terms arity"
    (let [atom (defs/create-atom "a" ["b" 5 true])]
      (is (= (defs/atom-arity atom) 3)))))

(deftest test-atom-ground
  "Test Atom ground checking functionality."
  
  (testing "Ground atom (no variables)"
    (let [atom (defs/create-atom "bird" ["tweety"])]
      (is (defs/atom-ground? atom))))
  
  (testing "Non-ground atom (has variables)"
    (let [atom (defs/create-atom "bird" ["X"])]
      (is (not (defs/atom-ground? atom))))))

(deftest test-atom-unification
  "Test Atom unification functionality."
  
  (testing "Successful unification"
    (let [atom1 (defs/create-atom "bird" ["X"])
          atom2 (defs/create-atom "bird" ["tweety"])
          result (defs/atom-unifies atom1 atom2)]
      (is (not (nil? result)))
      (is (= (get result "X") "tweety"))))
  
  (testing "Failed unification - different functors"
    (let [atom1 (defs/create-atom "bird" ["X"])
          atom2 (defs/create-atom "fish" ["tweety"])
          result (defs/atom-unifies atom1 atom2)]
      (is (nil? result))))
  
  (testing "Failed unification - different arity"
    (let [atom1 (defs/create-atom "bird" ["X"])
          atom2 (defs/create-atom "bird" ["tweety" "small"])
          result (defs/atom-unifies atom1 atom2)]
      (is (nil? result)))))

;; ============================================================================
;; Test Literal functionality - corresponds to Python TestLiteral class
;; ============================================================================

(deftest test-literal-parsing
  "Test Literal parsing functionality.
   Corresponds to Python TestLiteral.test__literal_parse__* methods (lines 199-210)."
  
  (testing "Parse positive literal"
    ;; Corresponds to test__literal_parse__0
    (let [expected (defs/create-literal false (defs/create-atom "a" []))
          result (core/parse-literal "a")]
      (is (= result expected))))
  
  (testing "Parse negative literal"
    ;; Corresponds to test__literal_parse__1
    (let [expected (defs/create-literal true (defs/create-atom "a" []))
          result (core/parse-literal "~a")]
      (is (= result expected))))
  
  (testing "Parse literal with terms"
    ;; Corresponds to test__literal_parse__2
    (let [expected (defs/create-literal false (defs/create-atom "a" ["b" 5 true]))
          result (core/parse-literal "a(b, 5, TRUE)")]
      (is (= result expected)))))

(deftest test-literal-creation
  "Test Literal creation and basic properties."
  
  (testing "Create positive literal"
    (let [atom (defs/create-atom "bird" ["tweety"])
          literal (defs/create-literal false atom)]
      (is (= (:negated literal) false))
      (is (= (:atom literal) atom))))
  
  (testing "Create negative literal"
    (let [atom (defs/create-atom "flies" ["tweety"])
          literal (defs/create-literal true atom)]
      (is (= (:negated literal) true))
      (is (= (:atom literal) atom)))))

(deftest test-literal-string-representation
  "Test Literal string representation."
  
  (testing "Positive literal"
    (let [atom (defs/create-atom "bird" ["tweety"])
          literal (defs/create-literal false atom)]
      (is (= (str literal) "bird(tweety)"))))
  
  (testing "Negative literal"
    (let [atom (defs/create-atom "flies" ["tweety"])
          literal (defs/create-literal true atom)]
      (is (= (str literal) "~flies(tweety)")))))

(deftest test-literal-properties
  "Test Literal property accessors."
  
  (testing "Literal functor access"
    (let [atom (defs/create-atom "bird" ["tweety"])
          literal (defs/create-literal false atom)]
      (is (= (defs/literal-functor literal) "bird"))))
  
  (testing "Literal terms access"
    (let [atom (defs/create-atom "bird" ["tweety"])
          literal (defs/create-literal false atom)]
      (is (= (defs/literal-terms literal) ["tweety"]))))
  
  (testing "Literal arity"
    (let [atom (defs/create-atom "bird" ["tweety"])
          literal (defs/create-literal false atom)]
      (is (= (defs/literal-arity literal) 1)))))

(deftest test-literal-complement
  "Test Literal complement functionality."
  
  (testing "Complement of positive literal"
    (let [atom (defs/create-atom "bird" ["tweety"])
          literal (defs/create-literal false atom)
          complement (defs/literal-complement literal)]
      (is (= (:negated complement) true))
      (is (= (:atom complement) atom))))
  
  (testing "Complement of negative literal"
    (let [atom (defs/create-atom "flies" ["tweety"])
          literal (defs/create-literal true atom)
          complement (defs/literal-complement literal)]
      (is (= (:negated complement) false))
      (is (= (:atom complement) atom)))))

(deftest test-literal-unification
  "Test Literal unification functionality."
  
  (testing "Successful unification - same polarity"
    (let [literal1 (defs/create-literal false (defs/create-atom "bird" ["X"]))
          literal2 (defs/create-literal false (defs/create-atom "bird" ["tweety"]))
          result (defs/literal-unifies literal1 literal2)]
      (is (not (nil? result)))
      (is (= (get result "X") "tweety"))))
  
  (testing "Failed unification - different polarity"
    (let [literal1 (defs/create-literal false (defs/create-atom "bird" ["X"]))
          literal2 (defs/create-literal true (defs/create-atom "bird" ["tweety"]))
          result (defs/literal-unifies literal1 literal2)]
      (is (nil? result)))))

;; ============================================================================
;; Test Rule functionality - corresponds to Python TestRule class
;; ============================================================================

(deftest test-rule-parsing
  "Test Rule parsing functionality."
  
  (testing "Parse strict rule with body"
    (let [result (core/parse-rule "bird(X) <- chicken(X).")]
      (is (defs/strict-rule? (:type result)))
      (is (= (count (:body result)) 1))))
  
  (testing "Parse defeasible rule with body"
    (let [result (core/parse-rule "flies(X) -< bird(X).")]
      (is (defs/defeasible-rule? (:type result)))
      (is (= (count (:body result)) 1))))
  
  (testing "Parse fact (strict rule with no body)"
    (let [result (core/parse-rule "chicken(tina).")]
      (is (defs/strict-rule? (:type result)))
      (is (empty? (:body result))))))

(deftest test-rule-creation
  "Test Rule creation and basic properties."
  
  (testing "Create strict rule"
    (let [head (defs/create-literal false (defs/create-atom "bird" ["X"]))
          body [(defs/create-literal false (defs/create-atom "chicken" ["X"]))]
          rule (defs/create-rule head (:strict defs/rule-types) body)]
      (is (= (:head rule) head))
      (is (= (:type rule) (:strict defs/rule-types)))
      (is (= (:body rule) body))))
  
  (testing "Create defeasible rule"
    (let [head (defs/create-literal false (defs/create-atom "flies" ["X"]))
          body [(defs/create-literal false (defs/create-atom "bird" ["X"]))]
          rule (defs/create-rule head (:defeasible defs/rule-types) body)]
      (is (= (:head rule) head))
      (is (= (:type rule) (:defeasible defs/rule-types)))
      (is (= (:body rule) body)))))

(deftest test-rule-properties
  "Test Rule property checking functionality."
  
  (testing "Fact identification"
    (let [head (defs/create-literal false (defs/create-atom "chicken" ["tina"]))
          rule (defs/create-rule head (:strict defs/rule-types) [])]
      (is (defs/rule-fact? rule))
      (is (not (defs/rule-presumption? rule)))))
  
  (testing "Presumption identification"
    (let [head (defs/create-literal false (defs/create-atom "assumption" ["X"]))
          rule (defs/create-rule head (:defeasible defs/rule-types) [])]
      (is (defs/rule-presumption? rule))
      (is (not (defs/rule-fact? rule)))))
  
  (testing "Regular rule identification"
    (let [head (defs/create-literal false (defs/create-atom "bird" ["X"]))
          body [(defs/create-literal false (defs/create-atom "chicken" ["X"]))]
          rule (defs/create-rule head (:strict defs/rule-types) body)]
      (is (not (defs/rule-fact? rule)))
      (is (not (defs/rule-presumption? rule))))))

(deftest test-rule-string-representation
  "Test Rule string representation."
  
  (testing "Fact representation"
    (let [head (defs/create-literal false (defs/create-atom "chicken" ["tina"]))
          rule (defs/create-rule head (:strict defs/rule-types) [])]
      (is (= (str rule) "chicken(tina)."))))
  
  (testing "Strict rule representation"
    (let [head (defs/create-literal false (defs/create-atom "bird" ["X"]))
          body [(defs/create-literal false (defs/create-atom "chicken" ["X"]))]
          rule (defs/create-rule head (:strict defs/rule-types) body)]
      (is (= (str rule) "bird(X) <- chicken(X)."))))
  
  (testing "Defeasible rule representation"
    (let [head (defs/create-literal false (defs/create-atom "flies" ["X"]))
          body [(defs/create-literal false (defs/create-atom "bird" ["X"]))]
          rule (defs/create-rule head (:defeasible defs/rule-types) body)]
      (is (= (str rule) "flies(X) -< bird(X).")))))

;; ============================================================================
;; Test Program functionality
;; ============================================================================

(deftest test-program-creation
  "Test Program creation and basic properties."
  
  (testing "Create empty program"
    (let [program (defs/create-program [])]
      (is (empty? (:rules program)))))
  
  (testing "Create program with rules"
    (let [rule1 (defs/create-rule 
                  (defs/create-literal false (defs/create-atom "chicken" ["tina"]))
                  (:strict defs/rule-types) [])
          rule2 (defs/create-rule
                  (defs/create-literal false (defs/create-atom "bird" ["X"]))
                  (:strict defs/rule-types)
                  [(defs/create-literal false (defs/create-atom "chicken" ["X"]))])
          program (defs/create-program [rule1 rule2])]
      (is (= (count (:rules program)) 2)))))

(deftest test-program-parsing
  "Test Program parsing from text."
  
  (testing "Parse simple program"
    (let [program-text "chicken(tina). bird(X) <- chicken(X)."
          program (core/parse-program program-text)]
      (is (= (count (:rules program)) 2))))
  
  (testing "Parse example program"
    (let [program (core/load-example-program)]
      (is (> (count (:rules program)) 0))
      (is (> (count (core/facts program)) 0))
      (is (> (count (core/strict-rules program)) 0))
      (is (> (count (core/defeasible-rules program)) 0)))))

(deftest test-program-analysis
  "Test Program analysis functions."
  
  (testing "Program facts extraction"
    (let [program (core/load-example-program)
          facts (core/facts program)]
      (is (> (count facts) 0))
      (is (every? defs/rule-fact? facts))))
  
  (testing "Program rules extraction by type"
    (let [program (core/load-example-program)
          strict-rules (core/strict-rules program)
          defeasible-rules (core/defeasible-rules program)]
      (is (> (count strict-rules) 0))
      (is (> (count defeasible-rules) 0))
      (is (every? #(defs/strict-rule? (:type %)) strict-rules))
      (is (every? #(defs/defeasible-rule? (:type %)) defeasible-rules)))))

;; ============================================================================
;; Test Variable Recognition
;; ============================================================================

(deftest test-variable-recognition
  "Test variable recognition functionality."
  
  (testing "Uppercase variables"
    (is (defs/variable? "X"))
    (is (defs/variable? "Variable"))
    (is (defs/variable? "X123")))
  
  (testing "Underscore variables"
    (is (defs/variable? "_"))
    (is (defs/variable? "_var"))
    (is (defs/variable? "_123")))
  
  (testing "Non-variables"
    (is (not (defs/variable? "lowercase")))
    (is (not (defs/variable? "123")))
    (is (not (defs/variable? "tina")))
    (is (not (defs/variable? 123)))
    (is (not (defs/variable? true)))))

;; ============================================================================
;; Test Value Recognition  
;; ============================================================================

(deftest test-value-recognition
  "Test value recognition functionality."
  
  (testing "Valid values"
    (is (defs/value? true))
    (is (defs/value? false))
    (is (defs/value? 123))
    (is (defs/value? 3.14))
    (is (defs/value? "string")))
  
  (testing "Invalid values" 
    (is (not (defs/value? nil)))
    (is (not (defs/value? [])))))

;; ============================================================================
;; Test Integration - End-to-End parsing and manipulation
;; ============================================================================

(deftest test-integration-parsing-and-manipulation
  "Integration test that verifies end-to-end functionality."
  
  (testing "Parse and manipulate complete program"
    (let [program-text "
          % Facts
          chicken(tina).
          penguin(tweety).
          
          % Strict rules  
          bird(X) <- chicken(X).
          bird(X) <- penguin(X).
          
          % Defeasible rules
          flies(X) -< bird(X).
          ~flies(X) -< penguin(X)."
          
          program (core/parse-program program-text)
          facts (core/facts program)
          strict-rules (core/strict-rules program) 
          defeasible-rules (core/defeasible-rules program)]
      
      ;; Verify parsing worked
      (is (> (count (:rules program)) 0))
      
      ;; Verify facts
      (is (= (count facts) 2))
      (is (every? defs/rule-fact? facts))
      
      ;; Verify strict rules  
      (is (= (count strict-rules) 2))
      (is (every? #(and (defs/strict-rule? (:type %))
                        (not (defs/rule-fact? %))) strict-rules))
      
      ;; Verify defeasible rules
      (is (= (count defeasible-rules) 2))
      (is (every? #(defs/defeasible-rule? (:type %)) defeasible-rules))
      
      ;; Verify string representation works
      (is (string? (str program)))
      (is (> (count (str program)) 0)))))

;; Run all tests when this namespace is loaded
(comment
  (run-tests 'depysible.definitions-test)) 