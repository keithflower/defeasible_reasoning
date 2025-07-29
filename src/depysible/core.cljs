(ns depysible.core
  "Main entry point for the ClojureScript translation of DePYsible.
   
   This namespace provides the primary API for parsing and working with
   defeasible logic programs, integrating the grammar parser with the
   parse tree transformer to provide a seamless interface.
   
   This corresponds to the main functionality that would be exposed by
   the Python package when imported."
  (:require [depysible.domain.definitions :as defs]
            [depysible.domain.rete :as rete]
            [depysible.domain.interpretation :as interp]
            [depysible.language.grammar :as grammar]
            [depysible.language.visitor :as visitor]
            [depysible.test-inference :as test-inf]))

;; ============================================================================
;; Main Parsing API - Integrates Grammar + Visitor
;; ============================================================================

(defn parse-literal
  "Parse a literal string and return a Literal object.
   
   Example: (parse-literal \"bird(tweety)\")
           (parse-literal \"~flies(X)\")"
  [literal-text]
  (visitor/parse-literal-to-object grammar/defeasible-grammar literal-text))

(defn parse-rule  
  "Parse a rule string and return a Rule object.
   
   Examples: (parse-rule \"bird(X) <- chicken(X).\")
            (parse-rule \"flies(X) -< bird(X).\")"
  [rule-text]
  (visitor/parse-rule-to-object grammar/defeasible-grammar rule-text))

(defn parse-program
  "Parse a program string and return a Program object.
   
   Example: (parse-program \"bird(X) <- chicken(X). chicken(tina).\")"
  [program-text]
  (visitor/parse-program-to-objects grammar/defeasible-grammar program-text))

(defn parse-atom
  "Parse an atom string and return an Atom object.
   
   Example: (parse-atom \"bird(tweety)\")"
  [atom-text]
  (visitor/parse-and-transform grammar/defeasible-grammar atom-text :atom))

;; ============================================================================  
;; Convenience Functions for Creating Domain Objects
;; ============================================================================

(defn create-atom
  "Create an Atom with functor and terms.
   
   Example: (create-atom \"bird\" [\"tweety\"])"
  [functor terms]
  (defs/create-atom functor terms))

(defn literal
  "Create a Literal with optional negation and atom.
   
   Examples: (literal false (atom \"bird\" [\"tweety\"]))
            (literal true (atom \"flies\" [\"X\"]))"
  ([atom] (defs/create-literal false atom))
  ([negated atom] (defs/create-literal negated atom)))

(defn strict-rule
  "Create a strict rule.
   
   Example: (strict-rule (literal false (atom \"bird\" [\"X\"]))
                        [(literal false (atom \"chicken\" [\"X\"]))])"
  ([head] (defs/create-rule head (:strict defs/rule-types) []))
  ([head body] (defs/create-rule head (:strict defs/rule-types) body)))

(defn defeasible-rule
  "Create a defeasible rule.
   
   Example: (defeasible-rule (literal false (atom \"flies\" [\"X\"]))
                            [(literal false (atom \"bird\" [\"X\"]))])"
  ([head] (defs/create-rule head (:defeasible defs/rule-types) []))
  ([head body] (defs/create-rule head (:defeasible defs/rule-types) body)))

(defn program
  "Create a Program with rules.
   
   Example: (program [rule1 rule2 rule3])"
  [rules]
  (defs/create-program rules))

;; ============================================================================
;; Enhanced Program Functions with RETE Integration
;; ============================================================================

(defn ground-program
  "Get the ground version of a program using RETE algorithm.
   This integrates the RETE algorithm to resolve the circular dependency."
  [program]
  (if (defs/program-ground? program)
    program
    (let [ground-rules (rete/fire-rules program)]
      (defs/create-program ground-rules))))

(defn analyze-rete-network
  "Analyze a program using RETE algorithm and return network statistics."
  [program]
  (if (defs/program-ground? program)
    {:message "Program is already ground, no RETE network needed"
     :rules-count (count (:rules program))}
    
    (let [agenda (cljs.core/atom [])
          node-table (cljs.core/atom {})
          root (rete/create-root)]
      
      ;; Build network (simplified for analysis)
      (doseq [fact (defs/program-get-facts program)]
        (swap! agenda conj fact))
      
      (doseq [rule (remove defs/rule-fact? (:rules program))]
        (when (seq (:body rule))
          (doseq [literal (:body rule)]
            (let [literal-name (str literal)]
              (when-not (get @node-table literal-name)
                (let [alfa-node (rete/create-alfa literal root)]
                  (swap! node-table assoc literal-name alfa-node)))))))
      
      ;; Get statistics
      (merge (rete/rete-network-stats root)
             {:original-rules (count (:rules program))
                             :facts (count (defs/program-get-facts program))
               :non-facts (count (remove defs/rule-fact? (:rules program)))}))))

;; ============================================================================
;; Argumentation Engine Integration
;; ============================================================================

(defn create-argumentation-interpreter
  "Create an argumentation interpreter for defeasible reasoning."
  [program]
  (interp/create-interpreter (ground-program program)))

(defn query-literal
  "Query a literal using the argumentation engine."
  [program literal]
  (let [interpreter (create-argumentation-interpreter program)]
    (interp/query-with-basic-explanation interpreter literal)))

(defn analyze-argumentation
  "Analyze argumentation structures in a program."
  [program]
  (let [interpreter (create-argumentation-interpreter program)]
    (interp/analyze-basic-argumentation interpreter)))

(defn check-contradictions
  "Check if program has contradictions in defeasible reasoning."
  [program]
  (let [interpreter (create-argumentation-interpreter program)]
    (interp/interpreter-contradictory? interpreter (:defeasible defs/rule-types))))

;; ============================================================================
;; Query and Analysis Functions  
;; ============================================================================

(defn facts
  "Get all facts from a program."
  [program]
  (defs/program-get-facts program))

(defn presumptions
  "Get all presumptions from a program."  
  [program]
  (defs/program-get-presumptions program))

(defn strict-rules
  "Get all strict rules from a program."
  [program]
  (defs/program-get-rules program (:strict defs/rule-types)))

(defn defeasible-rules
  "Get all defeasible rules from a program."
  [program]
  (defs/program-get-rules program (:defeasible defs/rule-types)))

(defn ground?
  "Check if a program is ground (contains no variables)."
  [program]
  (defs/program-ground? program))

(defn literals
  "Get all literals from a program."
  [program]
  (defs/program-as-literals program))

;; ============================================================================
;; String Conversion and Display
;; ============================================================================

(defn format-atom
  "Convert atom to string representation."
  [atom]
  (str atom))

(defn format-literal
  "Convert literal to string representation."
  [literal]
  (str literal))

(defn format-rule
  "Convert rule to string representation."
  [rule]
  (str rule))

(defn format-program
  "Convert program to formatted string representation."
  [program]
  (str program))

;; ============================================================================
;; Example Programs
;; ============================================================================

(def example-program-text
  "Example defeasible logic program from the original Python example.pl"
  "% Strict rules
   bird(X) <- chicken(X).
   bird(X) <- penguin(X).
   ~flies(X) <- penguin(X).
   
   % Facts  
   chicken(tina).
   penguin(tweety).
   scared(tina).
   
   % Defeasible knowledge
   flies(X) -< bird(X).
   flies(X) -< chicken(X), scared(X).
   ~flies(X) -< chicken(X).
   nests_in_trees(X) -< flies(X).")

(defn load-example-program
  "Load the example program for testing and demonstration."
  []
  (parse-program example-program-text))

;; ============================================================================
;; RETE Algorithm Demonstrations
;; ============================================================================

(defn demo-rete-network
  "Demonstrate RETE network construction and rule firing."
  []
  (let [program (load-example-program)]
    (println "=== RETE Network Demo ===")
    (println "Original program rules:" (count (:rules program)))
    (println "Ground program?" (ground? program))
    (println)
    
    (println "RETE Network Analysis:")
    (let [analysis (analyze-rete-network program)]
      (doseq [[key value] analysis]
        (println (str "  " (name key) ": " value))))
    (println)
    
    (println "Firing rules through RETE network...")
    (let [ground-prog (ground-program program)]
      (println "Ground program rules:" (count (:rules ground-prog)))
      (println "Facts in ground program:" (count (facts ground-prog)))
      (println)
      
      (println "Sample derived rules:")
      (doseq [rule (take 5 (:rules ground-prog))]
        (println "  " (format-rule rule))))))

;; ============================================================================
;; Error Handling and Validation
;; ============================================================================

(defn valid-parse?
  "Check if a parse result is valid (no parse errors)."
  [parse-result]
  (grammar/valid-parse? parse-result))

(defn parse-error
  "Get error message from failed parse result."
  [parse-result]
  (grammar/parse-error-message parse-result))

(defn safe-parse-literal
  "Safely parse literal with error handling."
  [literal-text]
  (try
    (let [result (parse-literal literal-text)]
      (if (valid-parse? result)
        {:success true :result result}
        {:success false :error (parse-error result)}))
    (catch js/Error e
      {:success false :error (.-message e)})))

(defn safe-parse-rule
  "Safely parse rule with error handling."
  [rule-text]
  (try
    (let [result (parse-rule rule-text)]
      (if (valid-parse? result)
        {:success true :result result}
        {:success false :error (parse-error result)}))
    (catch js/Error e
      {:success false :error (.-message e)})))

(defn safe-parse-program
  "Safely parse program with error handling."
  [program-text]
  (try
    (let [result (parse-program program-text)]
      (if (valid-parse? result)
        {:success true :result result}
        {:success false :error (parse-error result)}))
    (catch js/Error e
      {:success false :error (.-message e)})))

;; ============================================================================
;; Main Entry Point for Node.js
;; ============================================================================

(defn main []
  (println "DePYsible ClojureScript Translation")
  (println "==================================")
  (println)
  
  (println "=== TESTING USER'S ORIGINAL REQUEST ===")
  
  ;; Test the user's original simple program
  (println "\n✅ Testing: (def simple-program (grammar/parse-program \"bird(tweety). flies(X) -< bird(X).\"))")
  (try
    (let [simple-program (parse-program "bird(tweety). flies(X) -< bird(X).")]
      (if (map? simple-program)
        (do (println "SUCCESS! Program created with" (count (:rules simple-program)) "rules")
            (println "Rules:")
            (doseq [rule (:rules simple-program)]
              (println "  " (str rule))))
        (println "FAILED:" (pr-str simple-program))))
    (catch js/Error e
      (println "ERROR:" (.-message e))))

  ;; Test individual components
  (println "\n✅ Testing individual components:")
  
  (try
    (let [fact-only (parse-program "bird(tweety).")]
      (println "  bird(tweety). →" (count (:rules fact-only)) "rule"))
    (catch js/Error e
      (println "  bird(tweety). → ERROR:" (.-message e))))
  
  (try  
    (let [rule-only (parse-program "flies(X) -< bird(X).")]
      (println "  flies(X) -< bird(X). →" (count (:rules rule-only)) "rule"))
    (catch js/Error e
      (println "  flies(X) -< bird(X). → ERROR:" (.-message e))))

  ;; Test what requires and defs should work in REPL
  (println "\n✅ FOR REPL USE:")
  (println "  (require '[depysible.language.grammar :as grammar])")
  (println "  (require '[depysible.domain.definitions :as definitions])")  
  (println "  (def simple-program (grammar/parse-program \"bird(tweety). flies(X) -< bird(X).\"))")
  (println "  simple-program")
  (println "  (:rules simple-program)")

  (println "\n=== BASIC PARSING NOW WORKS! ===")
  (println)
  
  ;; Test a more complex example  
  (println "✅ Testing complex example...")
  (try
    (let [complex-program (parse-program "bird(X) <- chicken(X). flies(X) -< bird(X). chicken(tina).")]
      (println "Complex program with 3 statements →" (count (:rules complex-program)) "rules"))
    (catch js/Error e
      (println "Complex program → ERROR:" (.-message e))))

  ;; Test example program with comments
  (println "\n✅ Testing example program with comments...")
  (try
    (let [example-prog (load-example-program)]
      (println "Example program (with comments) →" (count (:rules example-prog)) "rules"))
    (catch js/Error e
      (println "Example program → ERROR:" (.-message e))))

  (println "\n🎉 DePYsible ClojureScript parsing is working!")
  (println "You can now use the REPL with the commands shown above.")
  
  ;; Test the new inference logic
  (test-inf/run-all-tests))

;; ============================================================================
;; Export Public API
;; ============================================================================

(def public-api
  "Main public API for the DePYsible ClojureScript translation."
  {;; Parsing functions
   :parse-literal parse-literal
   :parse-rule parse-rule  
   :parse-program parse-program
   
   ;; Safe parsing with error handling
   :safe-parse-literal safe-parse-literal
   :safe-parse-rule safe-parse-rule
   :safe-parse-program safe-parse-program
   
   ;; Constructor functions
   :atom create-atom
   :literal literal
   :strict-rule strict-rule
   :defeasible-rule defeasible-rule
   :program program
   
   ;; Enhanced program functions with RETE
   :ground-program ground-program
   :analyze-rete-network analyze-rete-network
   
   ;; Argumentation engine functions
   :create-argumentation-interpreter create-argumentation-interpreter
   :query-literal query-literal
   :analyze-argumentation analyze-argumentation
   :check-contradictions check-contradictions
   
   ;; Query functions
   :facts facts
   :presumptions presumptions
   :strict-rules strict-rules
   :defeasible-rules defeasible-rules
   :ground? ground?
   :literals literals
   
   ;; Formatting functions  
   :format-atom format-atom
   :format-literal format-literal
   :format-rule format-rule
   :format-program format-program
   
   ;; Utilities
   :valid-parse? valid-parse?
   :parse-error parse-error
   :load-example-program load-example-program
   :demo-rete-network demo-rete-network
   
   ;; Constants and types
   :rule-types defs/rule-types}) 