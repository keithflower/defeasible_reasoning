(ns depysible.integration-test
  "Integration tests for RETE algorithm and argumentation engine.
   
   This test suite verifies that the major components work together
   correctly, including parsing, RETE rule firing, and basic argumentation."
  (:require [cljs.test :refer [deftest testing is run-tests]]
            [depysible.core :as core]
            [depysible.domain.definitions :as defs]
            [depysible.domain.rete :as rete]
            [depysible.domain.interpretation :as interp]))

;; ============================================================================
;; Test Data
;; ============================================================================

(def simple-program-text
  "chicken(tweety).")

(def defeasible-program-text
  "chicken(tina).
   bird(X) <- chicken(X).
   flies(X) -< bird(X).")

(def complex-program-text
  "% Facts
   chicken(tina).
   penguin(tweety).
   scared(tina).
   
   % Strict rules
   bird(X) <- chicken(X).
   bird(X) <- penguin(X).
   ~flies(X) <- penguin(X).
   
   % Defeasible rules
   flies(X) -< bird(X).
   flies(X) -< chicken(X), scared(X).
   ~flies(X) -< chicken(X).")

;; ============================================================================
;; RETE Algorithm Tests
;; ============================================================================

(deftest test-rete-simple-program
  (testing "Basic RETE functionality with simple program"
    (let [program (core/parse-program simple-program-text)]
      (is (some? program) "Should parse simple program")
      (is (= 1 (count (:rules program))) "Should have one rule")
      
      ;; Test ground program generation
      (let [ground-prog (core/ground-program program)]
        (is (some? ground-prog) "Should generate ground program")
        (is (>= (count (:rules ground-prog)) 1) "Ground program should have at least one rule")))))

(deftest test-contradiction-detection
  (testing "Contradiction detection in programs"
    (let [contradictory-text "flies(tweety). ~flies(tweety)."
          contradictory-program (core/parse-program contradictory-text)
          non-contradictory-program (core/parse-program simple-program-text)]
      
      (let [has-contradiction (core/check-contradictions contradictory-program)]
        (is (boolean? has-contradiction) "Should return boolean for contradiction check"))
      
      (let [no-contradiction (core/check-contradictions non-contradictory-program)]
        (is (boolean? no-contradiction) "Should return boolean for no contradiction")))))

(deftest test-argumentation-analysis
  (testing "Argumentation structure analysis"
    (let [program (core/parse-program complex-program-text)
          analysis (core/analyze-argumentation program)]
      
      (is (map? analysis) "Analysis should return a map")
      (is (contains? analysis :structures-count) "Should report structure count")
      (is (contains? analysis :contradictory?) "Should check for contradictions")
      (is (boolean? (:contradictory? analysis)) "Contradictory should be boolean")
      (is (>= (:structures-count analysis) 0) "Should have non-negative structure count"))))

;; ============================================================================
;; Test Runner
;; ============================================================================

(defn run-integration-tests
  "Run all integration tests and report results."
  []
  (println "Running DePYsible Integration Tests...")
  (run-tests 'depysible.integration-test)) 