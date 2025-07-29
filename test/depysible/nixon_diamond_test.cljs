(ns depysible.nixon-diamond-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [depysible.core :refer [parse-program parse-literal ground-program]]
            [depysible.domain.interpretation :refer [create-interpreter query-with-full-explanation]]))

(def nixon-diamond-program-text
  "% Nixon Diamond Example
% Classic non-monotonic reasoning example with conflicting defaults
person(nixon).
quaker(nixon).
republican(nixon).

% Quakers are typically pacifists (higher salience)
pacifist(X) -< quaker(X) [12].

% Republicans are typically not pacifists (lower salience)
~pacifist(X) -< republican(X) [2].")

(deftest nixon-diamond-facts-test
  "Test that basic facts in the Nixon Diamond return YES."
  (testing "Basic facts should return YES"
    (let [program (parse-program nixon-diamond-program-text)
          grounded (ground-program program)
          interpreter (create-interpreter grounded program)]
      
      (testing "person(nixon) should return YES"
        (let [literal (parse-literal "person(nixon)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "YES") 
              (str "Expected person(nixon) to return YES, but got: " (:answer result)))))
      
      (testing "quaker(nixon) should return YES"
        (let [literal (parse-literal "quaker(nixon)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "YES") 
              (str "Expected quaker(nixon) to return YES, but got: " (:answer result)))))
      
      (testing "republican(nixon) should return YES"
        (let [literal (parse-literal "republican(nixon)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "YES") 
              (str "Expected republican(nixon) to return YES, but got: " (:answer result))))))))

(deftest nixon-diamond-reasoning-test
  "Test that salience-based reasoning resolves the Nixon Diamond correctly."
  (testing "Nixon Diamond salience resolution"
    (let [program (parse-program nixon-diamond-program-text)
          grounded (ground-program program)
          interpreter (create-interpreter grounded program)]
      
      (testing "pacifist(nixon) should return YES (quaker rule wins with salience 12)"
        (let [literal (parse-literal "pacifist(nixon)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "YES") 
              (str "Expected pacifist(nixon) to return YES (quaker rule has higher salience), but got: " 
                   (:answer result) " with explanation: " (:explanation result)))))
      
      (testing "~pacifist(nixon) should return NO (republican rule defeated by higher salience quaker rule)"
        (let [literal (parse-literal "~pacifist(nixon)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "NO") 
              (str "Expected ~pacifist(nixon) to return NO (republican rule is defeated), but got: " 
                   (:answer result) " with explanation: " (:explanation result))))))))

(deftest nixon-diamond-comprehensive-test
  "Comprehensive test of the Nixon Diamond with detailed salience verification."
  (testing "Complete Nixon Diamond functionality"
    (let [program (parse-program nixon-diamond-program-text)
          grounded (ground-program program)
          interpreter (create-interpreter grounded program)
          
          ;; Test all relevant literals
          person-result (query-with-full-explanation interpreter (parse-literal "person(nixon)"))
          quaker-result (query-with-full-explanation interpreter (parse-literal "quaker(nixon)"))
          republican-result (query-with-full-explanation interpreter (parse-literal "republican(nixon)"))
          pacifist-result (query-with-full-explanation interpreter (parse-literal "pacifist(nixon)"))
          not-pacifist-result (query-with-full-explanation interpreter (parse-literal "~pacifist(nixon)"))]
      
      (testing "All basic facts should return YES"
        (is (= (:answer person-result) "YES") "person(nixon) should be YES")
        (is (= (:answer quaker-result) "YES") "quaker(nixon) should be YES") 
        (is (= (:answer republican-result) "YES") "republican(nixon) should be YES"))
      
      (testing "Salience-based conflict resolution - quaker rule (salience 12) beats republican rule (salience 2)"
        (is (= (:answer pacifist-result) "YES") 
            (str "pacifist(nixon) should be YES (quaker rule wins), got: " (:answer pacifist-result)))
        (is (= (:answer not-pacifist-result) "NO") 
            (str "~pacifist(nixon) should be NO (republican rule loses), got: " (:answer not-pacifist-result))))
      
      (testing "No contradictions should exist in Nixon Diamond"
        ;; The key test: we should NOT have both pacifist(nixon) and ~pacifist(nixon) returning YES
        (is (not (and (= (:answer pacifist-result) "YES") 
                      (= (:answer not-pacifist-result) "YES")))
            "CONTRADICTION: Both pacifist(nixon) and ~pacifist(nixon) cannot be YES simultaneously"))
      
      (testing "Salience ordering is respected (12 > 2)"
        ;; Higher salience rule (quaker, 12) should win over lower salience (republican, 2)
        (is (= (:answer pacifist-result) "YES") "Higher salience rule should win")
        (is (= (:answer not-pacifist-result) "NO") "Lower salience rule should be defeated"))
      
      (testing "Results should be deterministic"
        ;; Test multiple times to ensure consistent results
        (let [pacifist-result-2 (query-with-full-explanation interpreter (parse-literal "pacifist(nixon)"))
              not-pacifist-result-2 (query-with-full-explanation interpreter (parse-literal "~pacifist(nixon)"))]
          (is (= (:answer pacifist-result) (:answer pacifist-result-2)) "pacifist(nixon) should be deterministic")
          (is (= (:answer not-pacifist-result) (:answer not-pacifist-result-2)) "~pacifist(nixon) should be deterministic")))))

(deftest nixon-diamond-salience-verification-test
  "Test that verifies the specific salience values are being used correctly."
  (testing "Salience value verification"
    (let [program (parse-program nixon-diamond-program-text)
          grounded (ground-program program)
          interpreter (create-interpreter grounded program)]
      
      (testing "Quaker rule should have higher precedence due to salience 12 > 2"
        ;; The pacifist(nixon) should win because quaker rule has salience 12
        ;; which is higher than republican rule salience 2
        (let [pacifist-result (query-with-full-explanation interpreter (parse-literal "pacifist(nixon)"))]
          (is (= (:answer pacifist-result) "YES")
              "Quaker rule with salience 12 should defeat republican rule with salience 2")))
      
      (testing "Republican rule should be defeated by higher salience quaker rule"
        ;; The ~pacifist(nixon) should lose because republican rule has salience 2
        ;; which is lower than quaker rule salience 12
        (let [not-pacifist-result (query-with-full-explanation interpreter (parse-literal "~pacifist(nixon)"))]
          (is (= (:answer not-pacifist-result) "NO")
              "Republican rule with salience 2 should be defeated by quaker rule with salience 12"))))))) 