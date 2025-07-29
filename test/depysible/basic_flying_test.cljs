(ns depysible.basic-flying-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [depysible.core :refer [parse-program parse-literal ground-program]]
            [depysible.domain.interpretation :refer [create-interpreter query-with-full-explanation]]))

(def basic-flying-program-text
  "% Basic Flying Example
% A simple example with chickens that normally fly but may be scared
bird(tina).
chicken(tina).
scared(tina).

% General rule: birds fly
flies(X) -< bird(X) [1].

% More specific rule: chickens don't fly (typically)
~flies(X) -< chicken(X) [2].

% Exception: scared chickens do fly
flies(X) -< chicken(X), scared(X) [3].")

(deftest basic-flying-facts-test
  "Test that basic facts in the Basic Flying example return YES."
  (testing "Basic facts should return YES"
    (let [program (parse-program basic-flying-program-text)
          grounded (ground-program program)
          interpreter (create-interpreter grounded program)]
      
      (testing "bird(tina) should return YES"
        (let [literal (parse-literal "bird(tina)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "YES") 
              (str "Expected bird(tina) to return YES, but got: " (:answer result) 
                   " with explanation: " (:explanation result)))))
      
      (testing "scared(tina) should return YES"
        (let [literal (parse-literal "scared(tina)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "YES") 
              (str "Expected scared(tina) to return YES, but got: " (:answer result)
                   " with explanation: " (:explanation result))))))))

(deftest basic-flying-salience-test
  "Test that salience-based reasoning works correctly in the Basic Flying example."
  (testing "Salience-based reasoning should work correctly"
    (let [program (parse-program basic-flying-program-text)
          grounded (ground-program program)
          interpreter (create-interpreter grounded program)]
      
      (testing "flies(tina) should return YES (scared chickens do fly - highest salience)"
        (let [literal (parse-literal "flies(tina)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "YES") 
              (str "Expected flies(tina) to return YES (scared chickens do fly), but got: " (:answer result)
                   " with explanation: " (:explanation result)))))
      
      (testing "~flies(tina) should return NO (defeated by higher salience)"
        (let [literal (parse-literal "~flies(tina)")
              result (query-with-full-explanation interpreter literal)]
          (is (= (:answer result) "NO") 
              (str "Expected ~flies(tina) to return NO (defeated by higher salience), but got: " (:answer result)
                   " with explanation: " (:explanation result))))))))

(deftest basic-flying-comprehensive-test
  "Comprehensive test combining all aspects of the Basic Flying example."
  (testing "Complete Basic Flying example functionality"
    (let [program (parse-program basic-flying-program-text)
          grounded (ground-program program)
          interpreter (create-interpreter grounded program)
          
          ;; Test all literals
          bird-result (query-with-full-explanation interpreter (parse-literal "bird(tina)"))
          chicken-result (query-with-full-explanation interpreter (parse-literal "chicken(tina)"))
          scared-result (query-with-full-explanation interpreter (parse-literal "scared(tina)"))
          flies-result (query-with-full-explanation interpreter (parse-literal "flies(tina)"))
          not-flies-result (query-with-full-explanation interpreter (parse-literal "~flies(tina)"))]
      
      (testing "All basic facts should return YES"
        (is (= (:answer bird-result) "YES") "bird(tina) should be YES")
        (is (= (:answer chicken-result) "YES") "chicken(tina) should be YES") 
        (is (= (:answer scared-result) "YES") "scared(tina) should be YES"))
      
      (testing "Salience resolution should work correctly"
        (is (= (:answer flies-result) "YES") 
            (str "flies(tina) should be YES (scared chickens do fly), got: " (:answer flies-result)))
        (is (= (:answer not-flies-result) "NO") 
            (str "~flies(tina) should be NO (defeated by higher salience), got: " (:answer not-flies-result))))
      
      (testing "No contradictions should exist"
        ;; The key test: we should NOT have both flies(tina) and ~flies(tina) returning YES
        (is (not (and (= (:answer flies-result) "YES") 
                      (= (:answer not-flies-result) "YES")))
            "CONTRADICTION: Both flies(tina) and ~flies(tina) cannot be YES simultaneously"))
      
      (testing "Results should be deterministic"
        ;; Test multiple times to ensure consistent results
        (let [flies-result-2 (query-with-full-explanation interpreter (parse-literal "flies(tina)"))
              not-flies-result-2 (query-with-full-explanation interpreter (parse-literal "~flies(tina)"))]
          (is (= (:answer flies-result) (:answer flies-result-2)) "flies(tina) should be deterministic")
          (is (= (:answer not-flies-result) (:answer not-flies-result-2)) "~flies(tina) should be deterministic")))))) 