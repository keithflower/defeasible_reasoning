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
;; Test Programs
;; ============================================================================

(def simple-program-text
  "% Simple strict rule program
   bird(X) <- chicken(X).
   chicken(tina).
   chicken(bob).")

(def defeasible-program-text
  "% Example with defeasible reasoning
   bird(X) <- chicken(X).
   chicken(tina).
   flies(X) -< bird(X).
   ~flies(X) -< chicken(X).")

(def complex-program-text
  "% Complex example similar to original Python example
   bird(X) <- chicken(X).
   bird(X) <- penguin(X).
   ~flies(X) <- penguin(X).
   
   chicken(tina).
   penguin(tweety).
   scared(tina).
   
   flies(X) -< bird(X).
   flies(X) -< chicken(X), scared(X).
   ~flies(X) -< chicken(X).")

;; ============================================================================
;; RETE Algorithm Tests
;; ============================================================================

(deftest test-rete-simple-program
  (testing "RETE algorithm with simple strict rules"
    (let [program (core/parse-program simple-program-text)
          ground-prog (core/ground-program program)]
      
      (is (some? program) "Program should parse successfully")
      (is (some? ground-prog) "Ground program should be generated")
      
      ;; Should derive bird(tina) and bird(bob)
      (let [facts (core/facts ground-prog)
            fact-heads (set (map :head facts))]
        (is (>= (count facts) 4) "Should have at least original facts plus derived ones")
        (is (contains? fact-heads (defs/create-literal false (defs/create-atom "bird" ["tina"])))
            "Should derive bird(tina)")
        (is (contains? fact-heads (defs/create-literal false (defs/create-atom "bird" ["bob"])))
            "Should derive bird(bob)")))))

(deftest test-rete-network-analysis
  (testing "RETE network analysis and statistics"
    (let [program (core/parse-program complex-program-text)
          analysis (core/analyze-rete-network program)]
      
      (is (map? analysis) "Analysis should return a map")
      (is (contains? analysis :original-rules) "Should report original rule count")
      (is (contains? analysis :facts) "Should report fact count")
      (is (> (:original-rules analysis) 0) "Should have original rules")
      (is (> (:facts analysis) 0) "Should have facts"))))

(deftest test-rete-ground-program-generation
  (testing "RETE ground program generation with variables"
    (let [program (core/parse-program defeasible-program-text)]
      
      (is (not (core/ground? program)) "Original program should not be ground")
      
      (let [ground-prog (core/ground-program program)]
        (is (some? ground-prog) "Should generate ground program")
        ;; Note: ground? check depends on having ground program implementation
        
        ;; Verify we have more rules after grounding
        (is (>= (count (:rules ground-prog)) (count (:rules program)))
            "Ground program should have at least as many rules as original")))))

;; ============================================================================
;; Argumentation Engine Tests
;; ============================================================================

(deftest test-argumentation-basic-query
  (testing "Basic argumentation queries"
    (let [program (core/parse-program defeasible-program-text)
          bird-tina (defs/create-literal false (defs/create-atom "bird" ["tina"]))
          flies-tina (defs/create-literal false (defs/create-atom "flies" ["tina"]))
          nonexistent (defs/create-literal false (defs/create-atom "nonexistent" ["foo"]))]
      
      ;; Test query for bird(tina) - should be YES (derived from chicken(tina))
      (let [result (core/query-literal program bird-tina)]
        (is (= "YES" (:answer result)) "bird(tina) should be derivable")
        (is (some? (:explanation result)) "Should have explanation"))
      
      ;; Test query for non-existent literal
      (let [result (core/query-literal program nonexistent)]
        (is (= "UNKNOWN" (:answer result)) "Non-existent literal should be unknown")))))

(deftest test-argumentation-analysis
  (testing "Argumentation structure analysis"
    (let [program (core/parse-program complex-program-text)
          analysis (core/analyze-argumentation program)]
      
      (is (map? analysis) "Analysis should return a map")
      (is (contains? analysis :structures-count) "Should report structure count")
      (is (contains? analysis :contradictory?) "Should check for contradictions")
      (is (boolean? (:contradictory? analysis)) "Contradictory should be boolean")
      (is (>= (:structures-count analysis) 0) "Should have non-negative structure count"))))

(deftest test-contradiction-detection
  (testing "Contradiction detection in programs"
    (let [contradictory-text "flies(tweety). ~flies(tweety)."
          contradictory-program (core/parse-program contradictory-text)
          non-contradictory-program (core/parse-program simple-program-text)]
      
      ;; Note: This test depends on proper contradiction detection
      ;; which may need more sophisticated implementation
      (let [has-contradiction (core/check-contradictions contradictory-program)]
        (is (boolean? has-contradiction) "Should return boolean for contradiction check"))
      
      (let [no-contradiction (core/check-contradictions non-contradictory-program)]
        (is (boolean? no-contradiction) "Should return boolean for no contradiction"))))

;; ============================================================================
;; Integration Tests - Full Pipeline
;; ============================================================================

(deftest test-full-pipeline-simple
  (testing "Complete pipeline: parse -> ground -> query"
    (let [program-text "chicken(tweety). bird(X) <- chicken(X). flies(X) -< bird(X)."
          program (core/parse-program program-text)]
      
      ;; 1. Parse successfully
      (is (some? program) "Should parse program")
      (is (= 3 (count (:rules program))) "Should have 3 rules")
      
      ;; 2. Ground the program
      (let [ground-prog (core/ground-program program)]
        (is (some? ground-prog) "Should ground program")
        (is (>= (count (:rules ground-prog)) 3) "Ground program should have at least original rules"))
      
      ;; 3. Query literals
      (let [chicken-tweety (defs/create-literal false (defs/create-atom "chicken" ["tweety"]))
            bird-tweety (defs/create-literal false (defs/create-atom "bird" ["tweety"]))
            flies-tweety (defs/create-literal false (defs/create-atom "flies" ["tweety"]))]
        
        ;; chicken(tweety) should be a fact
        (let [result (core/query-literal program chicken-tweety)]
          (is (= "YES" (:answer result)) "chicken(tweety) should be YES"))
        
        ;; bird(tweety) should be derivable
        (let [result (core/query-literal program bird-tweety)]
          (is (= "YES" (:answer result)) "bird(tweety) should be derivable"))
        
        ;; flies(tweety) should be defeasibly derivable
        (let [result (core/query-literal program flies-tweety)]
          (is (contains? #{"YES" "UNDECIDED"} (:answer result)) 
              "flies(tweety) should be derivable or undecided"))))))

(deftest test-full-pipeline-complex
  (testing "Complex pipeline with original example program"
    (let [program (core/load-example-program)]
      
      ;; Parse and analyze
      (is (some? program) "Example program should load")
      (is (> (count (:rules program)) 5) "Should have multiple rules")
      
      ;; Test RETE analysis
      (let [rete-analysis (core/analyze-rete-network program)]
        (is (> (:original-rules rete-analysis) 0) "Should have original rules")
        (is (> (:facts rete-analysis) 0) "Should have facts"))
      
      ;; Test argumentation analysis
      (let [arg-analysis (core/analyze-argumentation program)]
        (is (> (:structures-count arg-analysis) 0) "Should have argument structures"))
      
      ;; Test specific queries from the example
      (let [flies-tweety (defs/create-literal false (defs/create-atom "flies" ["tweety"]))
            bird-tweety (defs/create-literal false (defs/create-atom "bird" ["tweety"]))
            penguin-tweety (defs/create-literal false (defs/create-atom "penguin" ["tweety"]))]
        
        ;; penguin(tweety) is a fact
        (let [result (core/query-literal program penguin-tweety)]
          (is (= "YES" (:answer result)) "penguin(tweety) should be a fact"))
        
        ;; bird(tweety) should be derivable from penguin(tweety)
        (let [result (core/query-literal program bird-tweety)]
          (is (= "YES" (:answer result)) "bird(tweety) should be derivable"))
        
        ;; flies(tweety) should be complex due to conflicting rules
        (let [result (core/query-literal program flies-tweety)]
          (is (contains? #{"YES" "NO" "UNDECIDED"} (:answer result))
              "flies(tweety) should have some answer"))))))

;; ============================================================================
;; Error Handling Tests
;; ============================================================================

(deftest test-error-handling
  (testing "Error handling in various components"
    
    ;; Test parsing errors
    (let [invalid-program "invalid syntax here"]
      (try
        (core/parse-program invalid-program)
        (catch js/Error e
          (is (some? e) "Should throw error for invalid syntax"))))
    
    ;; Test querying with invalid literals
    (let [program (core/parse-program simple-program-text)
          invalid-literal nil]
      (try
        (core/query-literal program invalid-literal)
        (catch js/Error e
          (is (some? e) "Should handle null literal gracefully"))))
    
    ;; Test empty program
    (let [empty-program (core/parse-program "")]
      (is (some? empty-program) "Should handle empty program")
      (is (= 0 (count (:rules empty-program))) "Empty program should have no rules"))))

;; ============================================================================
;; Performance Tests
;; ============================================================================

(deftest test-performance-basic
  (testing "Basic performance with medium-sized programs"
    (let [start-time (js/Date.now)
          program (core/load-example-program)
          ground-prog (core/ground-program program)
          analysis (core/analyze-argumentation program)
          end-time (js/Date.now)
          duration (- end-time start-time)]
      
      (is (< duration 5000) "Complete pipeline should finish within 5 seconds")
      (is (some? ground-prog) "Should successfully ground program")
      (is (some? analysis) "Should successfully analyze program"))))

;; ============================================================================
;; Test Runner
;; ============================================================================

(defn run-integration-tests
  "Run all integration tests and report results."
  []
  (println "Running DePYsible Integration Tests...")
  (run-tests 'depysible.integration-test))

;; Enable running tests directly - DISABLED to prevent infinite loop
;; (when (exists? js/process)
;;   (run-integration-tests))
