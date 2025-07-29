(ns depysible.test-inference
  (:require [depysible.language.grammar :as grammar]
            [depysible.language.visitor :as visitor] 
            [depysible.domain.definitions :as defs]
            [depysible.domain.rete :as rete]))

(defn parse-program
  "Helper function to properly parse programs using visitor"
  [program-text]
  (visitor/parse-program-to-objects grammar/defeasible-grammar program-text))

(defn test-unification
  "Test that unification now works correctly"
  []
  (println "\n=== TESTING UNIFICATION ===")
  (let [chicken-x (defs/create-literal false (defs/create-atom "chicken" ["X"]))
        chicken-tina (defs/create-literal false (defs/create-atom "chicken" ["tina"]))
        unification-result (defs/literal-unifies chicken-x chicken-tina)]
    (println "Unifying chicken(X) with chicken(tina):")
    (println "  Result:" unification-result)
    (println "  Should be: {\"X\" \"tina\"}")
    (if (= unification-result {"X" "tina"})
      (println "  ✅ PASS: Unification working correctly!")
      (println "  ❌ FAIL: Unification broken"))))

(defn test-inference
  "Test that RETE inference now works correctly"
  []
  (println "\n=== TESTING RETE INFERENCE ===")
  ;; Debug different parsing patterns
  (println "\n=== PARSING DEBUG ===")
  (let [single-fact (parse-program "chicken(tina).")
        single-rule (parse-program "bird(X) <- chicken(X).")
        two-facts (parse-program "chicken(tina). chicken(bob).")
        test-program (parse-program "chicken(tina). chicken(bob). bird(X) <- chicken(X).")]
    
    (println "Single fact: chicken(tina). →" (count (:rules single-fact)) "rules")
    (println "Single rule: bird(X) <- chicken(X). →" (count (:rules single-rule)) "rules") 
    (println "Two facts: chicken(tina). chicken(bob). →" (count (:rules two-facts)) "rules")
    (println "Full program: chicken(tina). chicken(bob). bird(X) <- chicken(X). →" (count (:rules test-program)) "rules")
    
    (println "✅ Parsed program with" (count (:rules test-program)) "rules")
    
    ;; Test RETE firing
    (let [ground-rules (rete/fire-rules test-program)
          ground-program (defs/create-program ground-rules)
          facts (filter defs/rule-fact? (:rules ground-program))]
      
      (println "Ground program facts count:" (count facts))
      
      ;; Debug the program structure
      (println "\n=== PROGRAM STRUCTURE DEBUG ===")
      (println "Original program rules:")
      (doseq [rule (:rules test-program)]
        (println "  " (defs/rule->string rule) "- fact?" (defs/rule-fact? rule)))
      
      (println "\n=== RETE FIRING DEBUG ===")
      (println "Fire-rules returned:" (count ground-rules) "rules")
      (println "Ground rules:")
      (doseq [rule ground-rules]
        (println "  " (defs/rule->string rule)))
      
      (println "\nAll derived facts:")
      (doseq [rule facts]
        (println "  " (defs/rule->string rule)))
      
      (println "\n=== CHECKING EXPECTED RESULTS ===")
      (let [fact-strings (set (map defs/rule->string facts))
            expected #{"chicken(tina)." "chicken(bob)." "bird(tina)." "bird(bob)."}
            missing (clojure.set/difference expected fact-strings)
            unexpected (clojure.set/difference fact-strings expected)]
        
        (if (empty? missing)
          (println "✅ SUCCESS: All expected facts derived!")
          (do
            (println "❌ MISSING facts:")
            (doseq [fact missing]
              (println "    " fact))))
        
        (when (seq unexpected)
          (println "⚠️  UNEXPECTED facts:")
          (doseq [fact unexpected]
            (println "    " fact)))
        
        (empty? missing)))))

(defn run-all-tests
  "Run all inference tests"
  []
  (println "=== TESTING INFERENCE LOGIC ===")
  (test-unification)
  (test-inference)) 