(ns depysible.whitespace-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [depysible.core :refer [parse-program parse-literal ground-program]]
            [depysible.domain.interpretation :refer [create-interpreter query-with-full-explanation]]
            [depysible.language.grammar :as grammar]))

(deftest whitespace-in-atoms-test
  "Test that atoms can handle whitespace around parentheses and terms."
  (testing "Whitespace around parentheses in atoms"
    
    (testing "Space before opening parenthesis should work"
      (let [result (grammar/parse-literal "chicken (tina)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'chicken (tina)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Space after opening parenthesis should work"
      (let [result (grammar/parse-literal "chicken( tina)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'chicken( tina)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Space before closing parenthesis should work"
      (let [result (grammar/parse-literal "chicken(tina )")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'chicken(tina )' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Spaces around all parentheses should work"
      (let [result (grammar/parse-literal "chicken ( tina )")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'chicken ( tina )' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Multiple spaces should work"
      (let [result (grammar/parse-literal "chicken   (   tina   )")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'chicken   (   tina   )' but got error: " (grammar/parse-error-message result)))))))

(deftest whitespace-in-multiple-terms-test
  "Test that atoms with multiple terms handle whitespace correctly."
  (testing "Whitespace around commas in terms"
    
    (testing "Space after comma should work"
      (let [result (grammar/parse-literal "likes(tina, seeds)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'likes(tina, seeds)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Space before comma should work"
      (let [result (grammar/parse-literal "likes(tina , seeds)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'likes(tina , seeds)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Spaces around comma should work"
      (let [result (grammar/parse-literal "likes(tina , seeds)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'likes(tina , seeds)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Multiple spaces around comma should work"
      (let [result (grammar/parse-literal "likes(tina   ,   seeds)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'likes(tina   ,   seeds)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Complex whitespace with multiple terms"
      (let [result (grammar/parse-literal "relation ( arg1 , arg2 , arg3 )")]
        (is (grammar/valid-parse? result) 
            (str "Should parse 'relation ( arg1 , arg2 , arg3 )' but got error: " (grammar/parse-error-message result)))))))

(deftest whitespace-in-negation-test
  "Test that negated literals handle whitespace correctly."
  (testing "Whitespace after negation"
    
    (testing "Space after negation should work"
      (let [result (grammar/parse-literal "~ chicken(tina)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse '~ chicken(tina)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Multiple spaces after negation should work"
      (let [result (grammar/parse-literal "~   chicken(tina)")]
        (is (grammar/valid-parse? result) 
            (str "Should parse '~   chicken(tina)' but got error: " (grammar/parse-error-message result)))))
    
    (testing "Space after negation with spaces in atom"
      (let [result (grammar/parse-literal "~ chicken ( tina )")]
        (is (grammar/valid-parse? result) 
            (str "Should parse '~ chicken ( tina )' but got error: " (grammar/parse-error-message result)))))))

(deftest whitespace-in-complete-program-test
  "Test that complete programs with whitespace work correctly."
  (testing "Complete program with whitespace variations"
    (let [program-text "
      % Test program with various whitespace
      bird ( X ) <- chicken ( X ) .
      chicken ( tina ) .
      flies ( X ) -< bird ( X ) .
      ~ flies ( X ) <- injured ( X ) .
      injured ( tina ) .
      "]
      
      (testing "Program should parse successfully"
        (let [program (parse-program program-text)]
          (is (some? program) "Program should parse without errors")))
      
      (testing "Queries should work with whitespace"
        (let [program (parse-program program-text)
              grounded (ground-program program)
              interpreter (create-interpreter grounded program)]
          
          (testing "bird ( tina ) should return YES"
            (let [literal (parse-literal "bird ( tina )")
                  result (query-with-full-explanation interpreter literal)]
              (is (= (:answer result) "YES") 
                  (str "Expected bird ( tina ) to return YES, but got: " (:answer result)))))
          
          (testing "flies ( tina ) should return YES (bird rule applies, no injury rule)"
            (let [literal (parse-literal "flies ( tina )")
                  result (query-with-full-explanation interpreter literal)]
              (is (= (:answer result) "YES") 
                  (str "Expected flies ( tina ) to return YES, but got: " (:answer result))))))))))

(deftest whitespace-equivalence-test
  "Test that different whitespace variations produce equivalent results."
  (testing "Whitespace variations should be equivalent"
    (let [variations ["chicken(tina)"
                     "chicken (tina)"
                     "chicken( tina)"
                     "chicken(tina )"
                     "chicken ( tina )"
                     "chicken  (  tina  )"]]
      
      (doseq [variation variations]
        (testing (str "Variation: '" variation "'")
          (let [result (grammar/parse-literal variation)]
            (is (grammar/valid-parse? result) 
                (str "Should parse '" variation "' but got error: " (grammar/parse-error-message result)))))))))

(deftest whitespace-in-rules-test
  "Test that rules handle whitespace correctly in all positions."
  (testing "Whitespace in defeasible rules"
    
    (testing "Spaces around defeasible arrow"
      (let [result (grammar/parse-rule "flies ( X ) -< bird ( X ) .")]
        (is (grammar/valid-parse? result) 
            (str "Should parse defeasible rule with spaces but got error: " (grammar/parse-error-message result)))))
    
    (testing "Spaces around strict arrow"
      (let [result (grammar/parse-rule "bird ( X ) <- chicken ( X ) .")]
        (is (grammar/valid-parse? result) 
            (str "Should parse strict rule with spaces but got error: " (grammar/parse-error-message result)))))
    
    (testing "Complex rule with whitespace everywhere"
      (let [result (grammar/parse-rule "  flies ( X )  -<  bird ( X ) , ~ injured ( X )  [ 5 ]  .  ")]
        (is (grammar/valid-parse? result) 
            (str "Should parse complex rule with spaces but got error: " (grammar/parse-error-message result)))))))

(deftest whitespace-error-cases-test
  "Test that invalid syntax still produces errors even with whitespace."
  (testing "Invalid syntax should still fail"
    
    (testing "Missing closing parenthesis should fail"
      (let [result (grammar/parse-literal "chicken ( tina")]
        (is (not (grammar/valid-parse? result)) 
            "Should fail to parse missing closing parenthesis")))
    
    (testing "Missing opening parenthesis should fail"
      (let [result (grammar/parse-literal "chicken tina )")]
        (is (not (grammar/valid-parse? result)) 
            "Should fail to parse missing opening parenthesis")))
    
    (testing "Invalid functor should still fail"
      (let [result (grammar/parse-literal "123chicken ( tina )")]
        (is (not (grammar/valid-parse? result)) 
            "Should fail to parse invalid functor even with spaces"))))) 