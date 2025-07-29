(ns depysible.web
  "Web interface for the DePYsible defeasible logic system."
  (:require [depysible.core :refer [parse-program parse-literal parse-rule ground-program]]
            [depysible.domain.definitions :refer [rule-types program-get-facts program-get-strict 
                                                   program-get-defeasible program-get-rules]]
            [depysible.domain.interpretation :refer [create-interpreter interpreter-get-derivations
                                                     interpreter-get-structures interpreter-contradictory?
                                                     simple-query derivation-get-structure analyze-basic-argumentation
                                                     query-with-basic-explanation query-with-full-explanation
                                                     interpreter-get-defeaters interpreter-get-warrant]]
            [clojure.string :as str]))

;; ============================================================================
;; Program Analysis Functions
;; ============================================================================

(defn analyze-program
  "Analyze a parsed program and return detailed statistics."
  [program]
  (let [facts (program-get-facts program)
        strict-rules (program-get-rules program (:strict rule-types))
        defeasible-rules (program-get-rules program (:defeasible rule-types))
        total-rules (count (:rules program))
        
        analysis (str "Program Structure:\n"
                     "================\n"
                     "Total Rules: " total-rules "\n"
                     "Facts: " (count facts) "\n"
                     "Strict Rules: " (count strict-rules) "\n"
                     "Defeasible Rules: " (count defeasible-rules) "\n\n"
                     
                     "Facts:\n"
                     (str/join "\n" (map #(str "  " %) facts)) "\n\n"
                     
                     "Strict Rules:\n"
                     (str/join "\n" (map #(str "  " %) strict-rules)) "\n\n"
                     
                     "Defeasible Rules:\n"
                     (str/join "\n" (map #(str "  " %) defeasible-rules)))]
    
    {:total total-rules
     :facts (count facts)
     :strict (count strict-rules)
     :defeasible (count defeasible-rules)
     :analysis analysis}))

;; ============================================================================
;; Query Processing Functions
;; ============================================================================

(defn process-query-command
  "Process a query command from the REPL."
  [command program]
  (cond
    (str/starts-with? command "query ")
    (let [literal-str (str/trim (subs command 6))]
      (try
        (let [literal (parse-literal literal-str)
              grounded (ground-program program)
              interpreter (create-interpreter grounded program)
              result (query-with-full-explanation interpreter literal)]
          (str "Query: " literal-str "\n"
               "Result: " (:answer result) "\n"
               "Explanation: " (:explanation result) "\n"
               (when (:warrant result)
                 (str "Supporting Rules: " (str/join ", " (map str (:warrant result)))))))
        (catch js/Error e
          (str "Error parsing literal '" literal-str "': " (.-message e)))))
    
    (str/starts-with? command "derive ")
    (let [literal-str (str/trim (subs command 7))]
      (try
        (let [literal (parse-literal literal-str)
              grounded (ground-program program)
              interpreter (create-interpreter grounded program)
              derivations (interpreter-get-derivations interpreter literal (:defeasible rule-types))]
          (if (empty? derivations)
            (str "No derivations found for: " literal-str)
            (str "Derivations for " literal-str ":\n"
                 (str/join "\n" 
                   (map-indexed 
                     (fn [i deriv] 
                       (str "  " (inc i) ". " deriv))
                     derivations)))))
        (catch js/Error e
          (str "Error processing derivation for '" literal-str "': " (.-message e)))))
    
    (= command "structures")
    (try
      (let [grounded (ground-program program)
            interpreter (create-interpreter grounded program)
            structures (interpreter-get-structures interpreter (:defeasible rule-types))]
        (if (empty? structures)
          "No argument structures found."
          (str "Argument Structures (" (count structures) "):\n"
               (str/join "\n" 
                 (map-indexed 
                   (fn [i struct] 
                     (str "  " (inc i) ". Conclusion: " (:conclusion struct) "\n"
                          "     Argument: " (str/join ", " (map str (:argument struct)))))
                   structures)))))
      (catch js/Error e
        (str "Error analyzing structures: " (.-message e))))
    
    (= command "analyze")
    (try
      (let [grounded (ground-program program)
            interpreter (create-interpreter grounded program)
            analysis (analyze-basic-argumentation interpreter)
            contradictory? (interpreter-contradictory? interpreter (:defeasible rule-types))]
        (str "Detailed Program Analysis:\n"
             "=========================\n"
             "Argument Structures: " (:structures-count analysis) "\n"
             "Strict Structures: " (:strict-structures analysis) "\n"
             "Defeasible Structures: " (:defeasible-structures analysis) "\n"
             "Contradictory: " (if contradictory? "Yes" "No") "\n\n"
             "Sample Structures:\n"
             (str/join "\n" 
               (map-indexed 
                 (fn [i struct] 
                   (str "  " (inc i) ". " (:conclusion struct) " ← " 
                        (str/join ", " (map str (:argument struct)))))
                 (:sample-structures analysis)))))
      (catch js/Error e
        (str "Error analyzing program: " (.-message e))))
    
    (or (= command "help") (= command "?"))
    (str "Available Commands:\n"
         "==================\n"
         "query <literal>     - Query if a literal can be derived\n"
         "                      Example: query flies(tina)\n\n"
         "derive <literal>    - Show all derivations for a literal\n"
         "                      Example: derive flies(X)\n\n"
         "structures          - Show all argument structures\n\n"
         "analyze             - Detailed program analysis\n\n"
         "debug               - Run a simple debug test\n\n"
         "debug-defeaters     - Show defeater analysis for current program\n\n"
         "debug-warrants      - Show warrant computation for flies(tina) and ~flies(tina)\n\n"
         "help or ?           - Show this help message\n\n"
         "clear               - Clear REPL output\n\n"
         "Examples:\n"
         "  query flies(tina)\n"
         "  derive ~flies(X)\n"
         "  query nests_in_trees(tina)\n"
         "  debug\n"
         "  debug-defeaters\n"
         "  debug-warrants")
    
    (= command "clear")
    ""  ; This will be handled by the UI
    
    (= command "debug")
    (try
      (let [test-program (parse-program "chicken(tina). flies(X) -< chicken(X).")
            literal (parse-literal "flies(tina)")
            grounded (ground-program test-program)
            interpreter (create-interpreter grounded test-program)
            result (query-with-full-explanation interpreter literal)]
        (str "Debug Test Results:\n"
             "==================\n"
             "Simple test program: chicken(tina). flies(X) -< chicken(X).\n"
             "Query: flies(tina)\n"
             "Answer: " (:answer result) "\n"
             "Explanation: " (:explanation result) "\n"
             (when (:warrant result)
               (str "Warrant: " (str/join ", " (map str (:warrant result))) "\n"))
             "\nIf this shows +Δ, the system is working correctly."))
      (catch js/Error e
        (str "Debug error: " (.-message e))))
    
    (= command "debug-defeaters")
    (try
      (let [grounded (ground-program program)
            interpreter (create-interpreter grounded program)]
        (try
          (let [defeaters (interpreter-get-defeaters interpreter)]
            (if (empty? defeaters)
              "No defeaters found."
              (str "Defeater Analysis:\n"
                   "==================\n"
                   (str/join "\n" 
                     (mapcat 
                       (fn [[structure defeater-map]]
                         (cons (str "Structure: " (:conclusion structure))
                               (mapcat 
                                 (fn [[defeater-type type-map]]
                                   (map 
                                     (fn [[defeater disagreement]]
                                       (str "  " defeater-type " defeater: " (:conclusion defeater) " defeats " (:conclusion disagreement)))
                                     type-map))
                                 defeater-map)))
                       defeaters)))))
          (catch js/Error e
            (str "Error in defeater analysis: " (.-message e)))))
      (catch js/Error e
        (str "Error setting up interpreter: " (.-message e))))
    
    (= command "debug-warrants")
    (try
      (let [grounded (ground-program program)
            interpreter (create-interpreter grounded program)
            literal-pos (parse-literal "flies(tina)")
            literal-neg (parse-literal "~flies(tina)")]
        (try
          (let [warrant-pos (interpreter-get-warrant interpreter literal-pos (:defeasible rule-types))
                warrant-neg (interpreter-get-warrant interpreter literal-neg (:defeasible rule-types))]
            (str "Warrant Analysis:\n"
                 "=================\n"
                 "Warrant for flies(tina): " (if warrant-pos warrant-pos "NONE") "\n"
                 "Warrant for ~flies(tina): " (if warrant-neg warrant-neg "NONE") "\n\n"
                 "Expected: flies(tina) should have warrant, ~flies(tina) should NOT have warrant\n"
                 "Reason: 'flies(X) -< chicken(X), scared(X).' is more specific than '~flies(X) -< chicken(X).'"))
          (catch js/Error e
            (str "Error in warrant analysis: " (.-message e)))))
      (catch js/Error e
        (str "Error setting up warrant analysis: " (.-message e))))
    
    :else
    (str "Unknown command: '" command "'\nType 'help' for available commands.")))

;; ============================================================================
;; Web API Functions (called from JavaScript)
;; ============================================================================

(defn ^:export parse-program-web
  "Parse a program text and return analysis for the web interface."
  [program-text]
  (try
    (let [program (parse-program program-text)
          stats (analyze-program program)]
      ;; Store the program in a global var to avoid JS interop issues
      (set! (.-currentParsedProgram js/window) program)
      (clj->js {:success true
                :program "stored"  ; Just a marker
                :stats stats
                :analysis (:analysis stats)}))
    (catch js/Error e
      (clj->js {:success false
                :error (.-message e)}))))

(defn ^:export execute-command-web
  "Execute a REPL command and return the result."
  [command _unused-program]
  (let [program (.-currentParsedProgram js/window)]
    (if (nil? program)
      "No program loaded. Please parse a program first."
      (try
        (process-query-command (str/trim command) program)
        (catch js/Error e
          (str "Error executing command: " (.-message e)))))))

;; ============================================================================
;; Sample Programs Definition
;; ============================================================================

(def sample-programs
  {"Basic Flying"
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
flies(X) -< chicken(X), scared(X) [3]."

   "Penguin Paradox"
   "% Penguin Paradox - Classic Example
% Penguins are birds but don't fly
bird(tweety).
penguin(tweety).

% General rule: birds fly
flies(X) -< bird(X) [1].

% More specific rule: penguins don't fly
~flies(X) -< penguin(X) [2]."

   "Nixon Diamond"
   "% Nixon Diamond - Classic Ambiguity Example
% Nixon is both a Quaker and a Republican
person(nixon).
quaker(nixon).
republican(nixon).

% Quakers are typically pacifists
pacifist(X) -< quaker(X) [1].

% Republicans are typically not pacifists
~pacifist(X) -< republican(X) [1]."

   "Inheritance Example"
   "% Inheritance Example - Fee Payment Rules
% Students typically don't pay fees, but some do
person(alice).
person(bob).
student(alice).
student(bob).
faculty(bob).

% General rule: people pay fees
pay_fees(X) -< person(X) [1].

% Students typically don't pay fees
~pay_fees(X) -< student(X) [2].

% Faculty members always pay fees
pay_fees(X) <- faculty(X) [3]."

   "Complex Rules"
   "% Complex Rules Example - Multiple Interactions
% A more complex example with multiple overlapping rules
entity(x).
type_a(x).
type_b(x).
special_condition(x).

% Base rule
property_p(X) -< entity(X) [1].

% Override for type_a
~property_p(X) -< type_a(X) [2].

% Override for type_b
property_p(X) -< type_b(X) [3].

% Special condition overrides everything
~property_p(X) -< special_condition(X) [5]."

   "Salience Example"
   "% Salience Example - Numerical Priorities
% Demonstrates various salience values
bird(robin).
small_bird(robin).
injured(robin).

% Low priority: birds usually fly
flies(X) -< bird(X) [1].

% Medium priority: small birds don't fly well
~flies(X) -< small_bird(X) [3].

% High priority: injured birds definitely don't fly
~flies(X) -< injured(X) [10]."

   "Medical Diagnosis"
   "% Medical Diagnosis with Salience
% Realistic medical diagnosis example
patient(john).
has_fever(john).
has_cough(john).
has_fatigue(john).
elderly(john).
diabetic(john).

% General diagnostic rules
viral_infection(X) -< has_fever(X), has_cough(X) [2].
bacterial_infection(X) -< has_fever(X), has_cough(X), has_fatigue(X) [3].

% Age-based risk factors (higher priority)
pneumonia(X) -< elderly(X), has_cough(X), has_fever(X) [5].

% Comorbidity considerations (highest priority)
severe_infection(X) -< diabetic(X), bacterial_infection(X) [8].
requires_hospitalization(X) -< severe_infection(X) [8].

% Treatment recommendations based on diagnosis
antibiotic_treatment(X) -< bacterial_infection(X) [4].
monitoring_required(X) -< pneumonia(X) [6].
immediate_care(X) -< requires_hospitalization(X) [9]."

   "Automotive Diagnosis"
   "% Automotive Fault Diagnosis with Salience
% Realistic car diagnostic example
vehicle(car1).
engine_noise(car1).
poor_acceleration(car1).
high_fuel_consumption(car1).
high_mileage(car1).
overdue_maintenance(car1).

% Basic diagnostic rules
engine_problem(X) -< engine_noise(X) [2].
fuel_system_issue(X) -< poor_acceleration(X), high_fuel_consumption(X) [3].

% Mileage-based wear diagnosis (higher priority)
worn_engine(X) -< high_mileage(X), engine_noise(X) [5].
clogged_filter(X) -< high_mileage(X), poor_acceleration(X) [4].

% Maintenance history factors (highest priority)
severe_engine_damage(X) -< overdue_maintenance(X), worn_engine(X) [8].
immediate_repair(X) -< severe_engine_damage(X) [9].

% Repair recommendations
engine_overhaul(X) -< severe_engine_damage(X) [8].
filter_replacement(X) -< clogged_filter(X) [4].
routine_maintenance(X) -< fuel_system_issue(X) [3]."

   "Financial Risk Assessment"
   "% Financial Risk Assessment with Salience
% Realistic financial risk evaluation example
client(alice).
high_income(alice).
moderate_debt(alice).
good_credit_score(alice).
stable_employment(alice).
no_recent_defaults(alice).

% Basic risk assessment rules
low_risk(X) -< high_income(X) [2].
medium_risk(X) -< moderate_debt(X) [3].

% Credit score considerations (higher priority)
good_credit(X) -< good_credit_score(X) [5].
poor_credit(X) -< low_credit_score(X) [5].

% Employment and payment history (highest priority)
stable_borrower(X) -< stable_employment(X), no_recent_defaults(X) [7].
high_risk(X) -< recent_defaults(X) [8].

% Loan approval decisions
approve_loan(X) -< stable_borrower(X), good_credit(X) [6].
reject_loan(X) -< high_risk(X) [9].
requires_review(X) -< medium_risk(X), ~good_credit(X) [4]."}) 

;; ============================================================================
;; Sample Program API Functions
;; ============================================================================

(defn ^:export get-sample-programs []
  "Export sample programs to JavaScript"
  (clj->js sample-programs))

(defn ^:export get-sample-program [name]
  "Get a specific sample program by name"
  (get sample-programs name))

;; ============================================================================
;; Web API Initialization 
;; ============================================================================

(defn init-web-api []
  "Initialize the web API"
  (let [api (js-obj "parseProgram" parse-program-web
                   "executeCommand" execute-command-web
                   "getSamplePrograms" get-sample-programs
                   "getSampleProgram" get-sample-program)]
    (set! (.-depysibleWeb js/window) api)
    (println "Web API functions exported to window.depysibleWeb")
    (println "Available functions: parseProgram, executeCommand, getSamplePrograms, getSampleProgram")))

;; ============================================================================
;; Web Interface Initialization
;; ============================================================================

(defn ^:export init
  "Initialize the web interface."
  []
  (println "DePYsible web interface initialized!")
  
  ; Initialize global program storage
  (set! (.-currentParsedProgram js/window) nil)
  
  ; Set up global functions for JavaScript to call
  (init-web-api)
  
  (println "Web API functions exported to window.depysibleWeb")
  (println "Available functions: parseProgram, executeCommand, getSamplePrograms, getSampleProgram")) 