(ns depysible.domain.interpretation
  "Translation of src/main/python/depysible/domain/interpretation.py
   
   This namespace implements the argumentation engine for defeasible reasoning,
   including structures, dialectical trees, and the main interpreter for
   querying defeasible logic programs."
  (:require [clojure.set :as set]
            [depysible.domain.definitions :as defs]))

;; ============================================================================
;; Forward Declarations
;; ============================================================================

(declare create-index)
(declare get-derivations)
(declare index-contradictory?)
(declare literals-disagree?)
(declare interpreter-get-derivations)

;; ============================================================================
;; Helper Functions - correspond to Python utility functions (lines 442-512)
;; ============================================================================

(defn create-index
  "Create rule index for given mode.
   Corresponds to Python as_index function (lines 452-459)."
  ([rules] (create-index rules (:defeasible defs/rule-types)))
  ([rules mode]
   (let [index (atom {})]
     (doseq [rule rules]
       ;; Handle both keyword and numeric mode values
       (let [rule-type-val (:type rule)  ; This is already numeric (0 or 1)
             mode-val (if (keyword? mode)
                        (get defs/rule-types mode)  ; Convert keyword to numeric
                        mode)  ; Already numeric
             should-include (<= rule-type-val mode-val)]
         (when should-include
           (swap! index update (:head rule) (fnil conj #{}) rule))))
     @index)))

(defn rule-substitute
  "Apply substitutions to a rule.
   Creates a new rule with substitutions applied to head and body."
  [rule substitutions]
  (if (or (nil? substitutions) (empty? substitutions))
    rule
    (defs/->Rule 
      (defs/literal-substitute (:head rule) substitutions)
      (:type rule)
      (mapv #(defs/literal-substitute % substitutions) (:body rule))
      (:salience rule))))

(defn get-derivations
  "Get all derivations for a literal from index.
   Corresponds to Python get_derivations function (lines 471-500)."
  [literal index]
  ;; Find all index keys that unify with the target literal
  (let [derivations (atom [])
        unifying-keys (filter #(defs/literal-unifies % literal) (keys index))]
    (if (empty? unifying-keys)
      []
      (do
        (doseq [index-key unifying-keys]
          (let [substitution (defs/literal-unifies index-key literal)]
            (doseq [rule (get index index-key)]
                             ;; Apply substitution to the rule to ground it
               (let [grounded-rule (if substitution
                                    (rule-substitute rule substitution)
                                    rule)]
                 (let [partials (atom [[grounded-rule]])]
                   (doseq [body-literal (:body grounded-rule)]
                     (let [completions (get-derivations body-literal index)]
                       (if (empty? completions)
                         (reset! partials [])  ; Empty if any body literal can't be derived
                         (let [new-partials (atom [])]
                           (doseq [partial @partials]
                             (doseq [completion completions]
                               (let [combined-derivation (vec (concat partial completion))]
                                 ; Remove duplicates within the derivation
                                 (let [unique-derivation (vec (distinct combined-derivation))]
                                   (when-not (some #(= unique-derivation %) @new-partials)
                                     (swap! new-partials conj unique-derivation))))))
                           (reset! partials @new-partials)))))
                   ; Add successful derivations to final results
                   (doseq [partial @partials]
                     (when (seq partial)  ; Only add non-empty derivations
                       (when-not (some #(= partial %) @derivations)
                         (swap! derivations conj partial)))))))))
        @derivations))))

(defn index-contradictory?
  "Check if index is contradictory.
   Corresponds to Python is_contradictory function (lines 461-469)."
  [index]
  (loop [literals (keys index)]
    (if (seq literals)
      (let [literal (first literals)]
        (if (seq (get-derivations literal index))
          (let [complement (defs/literal-complement literal)]
            (if (seq (get-derivations complement index))
              true  ; Return true when contradiction found
              (recur (rest literals))))
          (recur (rest literals))))
      false)))  ; Return false when no contradiction found (instead of nil)

(defn literals-disagree?
  "Check if two literals disagree.
   Corresponds to Python disagree function (lines 442-450)."
  [literal1 literal2 rules]
  (if (= (defs/literal-complement literal1) literal2)
    true
    (let [index (create-index rules (:strict defs/rule-types))
          index-with-facts (-> index
                              (update literal1 (fnil conj #{}) (defs/literal-as-fact literal1))
                              (update literal2 (fnil conj #{}) (defs/literal-as-fact literal2)))]
      (index-contradictory? index-with-facts))))

;; ============================================================================
;; Enumerations - corresponds to Python enums
;; ============================================================================

(def defeater-type
  "Enumeration for types of defeaters.
   Corresponds to Python DefeaterType enum (lines 184-186)."
  {:proper 0
   :blocking 1})

(def mark-type
  "Enumeration for dialectical tree marking.
   Corresponds to Python Mark enum (lines 192-194)."
  {:undefeated 0
   :defeated 1})

(def answer-type
  "Enumeration for query answers.
   Corresponds to Python Answer enum (lines 309-314)."
  {:unknown -1
   :no 0
   :yes 1
   :undecided 2})

;; ============================================================================
;; Structure Record - corresponds to Python Structure class (lines 18-161)
;; ============================================================================

(defrecord ArgumentStructure [argument conclusion derivation])

(defn create-structure
  "Create an argument structure.
   Corresponds to Python Structure.__init__ method (lines 19-21)."
  [argument conclusion derivation]
  (->ArgumentStructure (set argument) conclusion derivation))

(defn structure-str
  "String representation of structure.
   Corresponds to Python Structure.__repr__ method (lines 27-36)."
  [structure]
  (if (empty? (:argument structure))
    (str "<∅, " (:conclusion structure) ">")
    (str "<{" (clojure.string/join " ; " (map str (:argument structure))) "}, " 
         (:conclusion structure) ">")))

(defn structure-strict?
  "Check if structure is strict (no defeasible rules).
   Corresponds to Python Structure.is_strict method (lines 120-121)."
  [structure]
  (empty? (:argument structure)))

(defn structure-subargument?
  "Check if structure is subargument of another.
   Corresponds to Python Structure.is_subargument_of method (lines 154-161)."
  [structure other-structure]
  (let [interp (:interpreter (:derivation structure))
        other-interp (:interpreter (:derivation other-structure))]
    (when-not (identical? interp other-interp)
      (throw (js/Error. "From different interpreters")))
    
    (set/subset? (:argument structure) (:argument other-structure))))

(defn structure-strictly-more-specific?
  "Check if structure is strictly more specific than another.
   Corresponds to Python Structure.is_strictly_more_specific_than method (lines 123-153)."
  [structure other-structure]
  (let [interp (:interpreter (:derivation structure))
        other-interp (:interpreter (:derivation other-structure))]
    (when-not (identical? interp other-interp)
      (throw (js/Error. "From different interpreters")))
    
    (let [derivables (atom #{})
          rules (atom #{})]
      ;; Collect derivables from grounded program
      (doseq [rule (:rules (:program interp))]
        (when (seq (interpreter-get-derivations interp (:head rule) (:defeasible defs/rule-types)))
          (swap! derivables conj (:head rule))))
      
      ;; Collect strict rules with body from ORIGINAL program (not grounded)
      (doseq [rule (:rules (:original-program interp))]
        (when (and (= (:type rule) (:strict defs/rule-types))
                   (seq (:body rule)))
          (swap! rules conj rule)))
      
      ;; Implement Python algorithm exactly using reduce for early termination
      (reduce
        (fn [more-specific derivable]
          (let [derivable-fact (defs/literal-as-fact derivable)
                sigma (create-index (conj @rules derivable-fact) (:strict defs/rule-types))
                delta1 (create-index (set/union @rules #{derivable-fact} (:argument structure)) (:defeasible defs/rule-types))
                delta2 (create-index (set/union @rules #{derivable-fact} (:argument other-structure)) (:defeasible defs/rule-types))
                
                def-conclusion (boolean (seq (get-derivations (:conclusion structure) delta1)))
                str-conclusion (boolean (seq (get-derivations (:conclusion structure) sigma)))
                def-structure (boolean (seq (get-derivations (:conclusion other-structure) delta2)))
                str-structure (boolean (seq (get-derivations (:conclusion other-structure) sigma)))
                
                condition-met (and def-structure (not str-structure) (not def-conclusion))]
            
            ;; Python: more_specific |= def_structure and not str_structure and not def_conclusion
            (let [updated-more-specific (or more-specific condition-met)]
              
              ;; Python: if not def_conclusion or not (not str_conclusion): continue
              ;; Python: if not def_structure: return False
              (if (and def-conclusion (not str-conclusion) (not def-structure))
                (reduced false)  ; Early termination with false
                updated-more-specific))))
        false
        @derivables))))

(defn structure-counter-argument?
  "Check if structure is counter argument of another with respect to disagreement.
   Corresponds to Python Structure.is_counter_argument_of method (lines 38-48)."
  [structure other-structure disagreement]
  (let [interp (:interpreter (:derivation structure))
        other-interp (:interpreter (:derivation other-structure))
        disagr-interp (:interpreter (:derivation disagreement))]
    (when-not (and (identical? interp other-interp)
                   (identical? interp disagr-interp))
      (throw (js/Error. "From different interpreters")))
    
    (if (or (structure-strict? structure) (structure-strict? other-structure))
      false
      (and (structure-subargument? disagreement other-structure)
           (literals-disagree? (:conclusion disagreement) (:conclusion structure)
                             (:rules (:program interp)))))))

(defn structure-equi-specific?
  "Check if structure is equi-specific to another.
   Corresponds to Python Structure.is_equi_specific_to method (lines 50-72).
   Modified to handle salience-based comparisons for contradictory conclusions."
  [structure other-structure]
  (let [interp (:interpreter (:derivation structure))
        other-interp (:interpreter (:derivation other-structure))]
    (when-not (identical? interp other-interp)
      (throw (js/Error. "From different interpreters")))
    
    ;; Check if both structures have defeasible arguments (non-empty argument sets)
    (let [both-have-defeasible-args? (and (not (empty? (:argument structure)))
                                          (not (empty? (:argument other-structure))))
          contradictory-conclusions? (literals-disagree? (:conclusion structure) 
                                                        (:conclusion other-structure)
                                                        (:rules (:program interp)))]
      
      (cond
        ;; If arguments are identical, they are equi-specific (original logic)
        (= (:argument structure) (:argument other-structure))
        true
        
        ;; If both have defeasible arguments and contradictory conclusions, use salience
        (and both-have-defeasible-args? contradictory-conclusions?)
        true
        
        ;; If either has empty argument, they are equi-specific only if both are empty
        (or (empty? (:argument structure)) (empty? (:argument other-structure)))
        (and (empty? (:argument structure)) (empty? (:argument other-structure)))
        
        ;; If both have non-empty arguments but non-contradictory conclusions, they're not equi-specific
        both-have-defeasible-args?
        false
        
        ;; Otherwise, use the original complex logic for specificity checking
        :else
        (let [index1 (create-index (conj (:rules (:program interp)) 
                                         (defs/literal-as-fact (:conclusion structure))) 
                                   (:strict defs/rule-types))
              derivations1 (get-derivations (:conclusion other-structure) index1)
              
              index2 (create-index (conj (:rules (:program interp)) 
                                         (defs/literal-as-fact (:conclusion other-structure))) 
                                   (:strict defs/rule-types))
              derivations2 (get-derivations (:conclusion structure) index2)]
          (and (seq derivations1) (seq derivations2)))))))

(defn structure-more-salient?
  "Check if structure is more salient than another.
   Simplified approach: compare maximum salience values."
  [structure other-structure]
  (let [max-sal-1 (if (empty? (:argument structure)) 
                    0 
                    (apply max (map :salience (:argument structure))))
        max-sal-2 (if (empty? (:argument other-structure)) 
                    0 
                    (apply max (map :salience (:argument other-structure))))]
    (> max-sal-1 max-sal-2)))

(defn structure-preferable?
  "Check if structure is preferable to another.
   Corresponds to Python Structure.is_preferable_to method (lines 84-88)."
  [structure other-structure]
  (if (structure-equi-specific? structure other-structure)
    (structure-more-salient? structure other-structure)
    (structure-strictly-more-specific? structure other-structure)))

(defn structure-proper-defeater?
  "Check if structure is proper defeater for another with respect to disagreement.
   Corresponds to Python Structure.is_proper_defeater_for method (lines 89-98)."
  [structure target-structure disagreement]
  (let [interp (:interpreter (:derivation structure))
        target-interp (:interpreter (:derivation target-structure))
        disagr-interp (:interpreter (:derivation disagreement))]
    (when-not (and (identical? interp target-interp)
                   (identical? interp disagr-interp))
      (throw (js/Error. "From different interpreters")))
    
    (and (structure-subargument? disagreement target-structure)
         (structure-counter-argument? structure target-structure disagreement)
         (structure-preferable? structure disagreement))))

(defn structure-blocking-defeater?
  "Check if structure is blocking defeater for another with respect to disagreement.
   Corresponds to Python Structure.is_blocking_defeater_for method (lines 99-109)."
  [structure target-structure disagreement]
  (let [interp (:interpreter (:derivation structure))
        target-interp (:interpreter (:derivation target-structure))
        disagr-interp (:interpreter (:derivation disagreement))]
    (when-not (and (identical? interp target-interp)
                   (identical? interp disagr-interp))
      (throw (js/Error. "From different interpreters")))
    
    (and (structure-subargument? disagreement target-structure)
         (structure-counter-argument? structure target-structure disagreement)
         (not (structure-preferable? structure disagreement))
         (not (structure-preferable? disagreement structure)))))

(defn structure-defeater?
  "Check if structure is defeater for another with respect to disagreement.
   Corresponds to Python Structure.is_defeater_for method (lines 110-118)."
  [structure target-structure disagreement]
  (let [interp (:interpreter (:derivation structure))
        target-interp (:interpreter (:derivation target-structure))
        disagr-interp (:interpreter (:derivation disagreement))]
    (when-not (and (identical? interp target-interp)
                   (identical? interp disagr-interp))
      (throw (js/Error. "From different interpreters")))
    
    (or (structure-proper-defeater? structure target-structure disagreement)
        (structure-blocking-defeater? structure target-structure disagreement))))

;; ============================================================================
;; Derivation Record - corresponds to Python Derivation class (lines 166-181)
;; ============================================================================

(defrecord ArgumentDerivation [rules interpreter])

(defn create-derivation
  "Create a derivation.
   Corresponds to Python Derivation.__init__ method (lines 167-168)."
  [rules interpreter]
  (->ArgumentDerivation rules interpreter))

(defn derivation-str
  "String representation of derivation.
   Corresponds to Python Derivation.__repr__ method (lines 173-178)."
  [derivation]
  (let [explanation (if (seq (:rules derivation))
                     (clojure.string/join ", " (map #(str (:head %)) (reverse (:rules derivation))))
                     "∅")
        symbol (if (some #(= (:type %) (:defeasible defs/rule-types)) (:rules derivation))
                 "|~"
                 "|-")
        derivable (if (seq (:rules derivation))
                   (:head (first (:rules derivation)))
                   "")]
    (str explanation " " symbol " " derivable)))

(defn derivation-get-structure
  "Get structure from derivation.
   Corresponds to Python Derivation.get_structure method (lines 180-181)."
  [derivation]
  (let [;; Map derived facts back to original template rules
        original-rules (atom #{})
        conclusion (if (seq (:rules derivation))
                    (:head (first (:rules derivation)))
                    nil)]
    
    ;; For each rule in the derivation, find the corresponding original template rule
    (doseq [derived-rule (:rules derivation)]
      ;; Only process defeasible rules for argument structure
      ;; Strict rules (facts) should result in empty argument sets
      (when (= (:type derived-rule) (:defeasible defs/rule-types))
        (if (empty? (:body derived-rule))
          ;; This is a derived fact - find the most specific matching original template rule
          (let [original-program-rules (:rules (:original-program (:interpreter derivation)))
                grounded-facts (set (map :head (filter #(empty? (:body %)) (:rules (:program (:interpreter derivation))))))
                head-literal (:head derived-rule)
                matching-rules (atom [])]
            
            ;; Find all template rules that could have derived this head
            (doseq [template-rule original-program-rules]
              (when (and (= (:type template-rule) (:defeasible defs/rule-types))
                         (seq (:body template-rule))  ; Has body (not a fact)
                         ;; Check if this template rule could unify with the derived head
                         (defs/literal-unifies (:head template-rule) head-literal))
                
                ;; Check if all body conditions of this rule are satisfied by available facts
                (let [body-conditions (:body template-rule)
                      all-conditions-satisfied? 
                      (every? (fn [body-literal]
                                ;; Check if this body condition can be satisfied by grounded facts
                                (some #(defs/literal-unifies body-literal %) grounded-facts))
                              body-conditions)]
                  (when all-conditions-satisfied?
                    (swap! matching-rules conj template-rule)))))
            
            ;; Choose the most specific rule (one with most body conditions)
            (when (seq @matching-rules)
              (let [most-specific-rule (apply max-key #(count (:body %)) @matching-rules)]
                (swap! original-rules conj most-specific-rule)
                
                ;; Also include dependency rules for body literals 
                (doseq [body-literal (:body most-specific-rule)]
                  (doseq [template-rule original-program-rules]
                    (when (and (= (:type template-rule) (:defeasible defs/rule-types))
                               (seq (:body template-rule))
                               (defs/literal-unifies (:head template-rule) body-literal)
                               ;; Check if this dependency rule's conditions are satisfied
                               (every? (fn [dep-body-literal]
                                        (some #(defs/literal-unifies dep-body-literal %) grounded-facts))
                                      (:body template-rule)))
                      (swap! original-rules conj template-rule)))))))
          ;; This rule still has a body - use it as is
          (swap! original-rules conj derived-rule))))
    
    (create-structure @original-rules conclusion derivation)))

;; ============================================================================
;; Simplified Interpreter - corresponds to Python Interpreter class (lines 316-441)
;; ============================================================================

(defrecord DefeasibleInterpreter [program original-program])

(defn create-interpreter
  "Create a defeasible interpreter.
   Corresponds to Python Interpreter.__init__ method (lines 320-326)."
  ([program] (create-interpreter program program))  ; For backward compatibility
  ([program original-program]
   ;; Store both grounded program (for reasoning) and original program (for arguments)
   (->DefeasibleInterpreter program original-program)))

(defn interpreter-get-derivations
  "Get derivations for a literal.
   Corresponds to Python Interpreter.get_derivations method (lines 364-372)."
  [interpreter literal mode]
  (if (empty? (:rules (:program interpreter)))
    #{}
    (let [index (create-index (:rules (:program interpreter)) mode)]
      (if-not (contains? index literal)
        #{}
        (set (map #(create-derivation % interpreter)
                 (get-derivations literal index)))))))

(defn interpreter-get-literals
  "Get all literals for mode.
   Corresponds to Python Interpreter.get_literals method (lines 374-378)."
  [interpreter mode]
  (set (map :head (filter #(<= (get defs/rule-types (:type %))
                               (get defs/rule-types mode))
                         (:rules (:program interpreter))))))

(defn interpreter-get-structures
  "Get all structures for mode.
   Corresponds to Python Interpreter.get_structures method (lines 380-393)."
  [interpreter mode]
  (let [structures (atom #{})
        literals (interpreter-get-literals interpreter mode)]
    (doseq [literal literals]
      (let [derivations (interpreter-get-derivations interpreter literal mode)]
        (doseq [derivation derivations]
          (let [structure (derivation-get-structure derivation)]
            (swap! structures conj structure)))))
    @structures))

(defn interpreter-contradictory?
  "Check if interpreter is contradictory.
   Corresponds to Python Interpreter.is_contradictory method (lines 395-398)."
  [interpreter mode]
  ;; Only check for contradictions in strict mode
  ;; In defeasible logic, having both p and ~p derivable is not a contradiction
  (if (= mode (:strict defs/rule-types))
    (let [index (create-index (:rules (:program interpreter)) mode)]
      (index-contradictory? index))
    false))

(defn simple-query
  "Simple query implementation without full dialectical argumentation.
   This provides basic defeasible reasoning without defeat analysis."
  [interpreter literal mode]
  ;; Try to get derivations directly instead of checking literals set first
  (let [derivations (interpreter-get-derivations interpreter literal mode)]
    (if (seq derivations)
      (let [structure (derivation-get-structure (first derivations))]
        [1 (:argument structure)])
      (let [complement (defs/literal-complement literal)
            complement-derivations (interpreter-get-derivations interpreter complement mode)]
        (if (seq complement-derivations)
          [0 (:argument (derivation-get-structure (first complement-derivations)))]
          ;; Only return UNKNOWN if we can't find the literal in the program at all
          (let [literals (interpreter-get-literals interpreter mode)]
            (if (or (contains? literals literal) (contains? literals complement))
              [2 nil]  ; UNDECIDED - literal is in program but neither it nor complement can be derived
              [-1 nil])))))))

;; ============================================================================
;; Analysis and Utility Functions
;; ============================================================================

(defn analyze-basic-argumentation
  "Basic analysis of argumentation structures in an interpreter."
  [interpreter]
  (let [structures (interpreter-get-structures interpreter (:defeasible defs/rule-types))]
    {:structures-count (count structures)
     :strict-structures (count (filter structure-strict? structures))
     :defeasible-structures (count (remove structure-strict? structures))
     :contradictory? (interpreter-contradictory? interpreter (:defeasible defs/rule-types))
     :sample-structures (take 5 structures)}))

(defn query-with-basic-explanation
  "Query with basic explanation of reasoning."
  [interpreter literal]
  (let [result (simple-query interpreter literal (:defeasible defs/rule-types))
        [answer warrant] result]
    {:literal literal
     :answer (case answer
               -1 "UNKNOWN"
               1 "YES"
               0 "NO"
               2 "UNDECIDED"
               "UNKNOWN")
     :warrant warrant
     :explanation (case answer
                   -1 "Literal not derivable in program"
                   1 "Literal has derivation"
                   0 "Literal's complement has derivation"
                   2 "Neither literal nor complement derivable"
                   "Unknown answer type")}))

;; Forward declarations for dialectical tree functions to avoid compilation warnings
(declare dialectical-tree-build
         dialectical-tree-acceptable?
         dialectical-tree-already-used?
         dialectical-tree-subargument?
         dialectical-tree-concordant?
         dialectical-tree-not-chain-or-defeated?)

;; ============================================================================
;; Dialectical Tree - corresponds to Python DialecticalTree class (lines 198-308)
;; ============================================================================

(defrecord DialecticalTree [content disagreement parent children])

(defn create-dialectical-tree
  "Create a dialectical tree node.
   Corresponds to Python DialecticalTree.__init__ method (lines 211-222)."
  ([structure] (create-dialectical-tree structure nil nil))
  ([structure disagreement parent]
   (->DialecticalTree structure disagreement parent #{})))

(defn dialectical-tree-create
  "Create dialectical tree for structure.  
   Corresponds to Python DialecticalTree.create static method (lines 204-208)."
  [structure defeaters]
  (let [tree (create-dialectical-tree structure)]
    (dialectical-tree-build tree defeaters)))

(defn dialectical-tree-build
  "Build dialectical tree with defeaters.
   Corresponds to Python DialecticalTree.build method (lines 235-241)."
  [tree defeaters]
  (if-let [structure-defeaters (get defeaters (:content tree))]
    (let [new-children (atom (:children tree))]
      (doseq [[defeater-type defeater-map] structure-defeaters]
        (doseq [[defeater disagreement] defeater-map]
          (when (dialectical-tree-acceptable? tree defeater disagreement)
            (let [child (create-dialectical-tree defeater disagreement tree)
                  updated-child (dialectical-tree-build child defeaters)]
              (swap! new-children conj updated-child)))))
      ;; Return updated tree with new children
      (assoc tree :children @new-children))
    ;; No defeaters, return original tree
    tree))

(defn dialectical-tree-acceptable?
  "Check if defeater is acceptable for tree.
   Corresponds to Python DialecticalTree.is_acceptable method (lines 243-251)."
  [tree defeater disagreement]
  (and (not (dialectical-tree-already-used? tree defeater))
       (not (dialectical-tree-subargument? tree defeater))
       (not (dialectical-tree-concordant? tree defeater))
       (not (dialectical-tree-not-chain-or-defeated? tree defeater disagreement))))

(defn dialectical-tree-already-used?
  "Check if defeater is already used in tree.
   Corresponds to Python DialecticalTree._is_already_used method (lines 253-260)."
  [tree defeater]
  (if (nil? (:parent tree))
    false
    (if (= (:content tree) defeater)
      true
      (dialectical-tree-already-used? (:parent tree) defeater))))

(defn dialectical-tree-subargument?
  "Check if defeater is subargument of tree content.
   Corresponds to Python DialecticalTree._is_subargument method (lines 262-270)."
  [tree defeater]
  (structure-subargument? defeater (:content tree)))

(defn dialectical-tree-concordant?
  "Check if defeater is concordant with tree content.
   Corresponds to Python DialecticalTree._is_concordant method (lines 271-284)."
  [tree defeater]
  (let [defeater-conclusion (:conclusion defeater)
        content-conclusion (:conclusion (:content tree))]
    (not (literals-disagree? defeater-conclusion content-conclusion 
                            (set/union (:argument defeater) (:argument (:content tree)))))))

(defn dialectical-tree-not-chain-or-defeated?
  "Check if defeater creates chain or is defeated.
   Corresponds to Python DialecticalTree._is_not_chain_or_defeated method (lines 285-293)."
  [tree defeater disagreement]
  ;; Simplified implementation - in full system would check for cycles and defeat
  false)

(defn dialectical-tree-mark
  "Mark dialectical tree as defeated or undefeated.
   Corresponds to Python DialecticalTree.mark method (lines 294-308)."
  [tree]
  (if (empty? (:children tree))
    (:undefeated mark-type)
    (let [child-marks (map dialectical-tree-mark (:children tree))
          all-defeated? (every? #(= % (:defeated mark-type)) child-marks)]
      (if all-defeated?
        (:undefeated mark-type)
        (:defeated mark-type)))))

;; ============================================================================
;; Defeater Analysis - corresponds to Python Interpreter.get_defeaters (lines 328-363)
;; ============================================================================

(defn interpreter-get-defeaters
  "Get defeaters for all arguments in interpreter.
   Corresponds to Python Interpreter.get_defeaters method (lines 328-363)."
  [interpreter]
  (let [arguments (interpreter-get-structures interpreter (:defeasible defs/rule-types))
        
        ;; Build subargument relationships
        subarguments (atom {})
        _ (doseq [argument arguments]
            (doseq [subargument arguments]
              (when (structure-subargument? subargument argument)
                (swap! subarguments update argument (fnil conj #{}) subargument))))
        
        ;; Build counter-argument relationships
        counter-arguments (atom {})
        _ (doseq [counter-argument arguments]
            (doseq [argument arguments]
              (when (not= counter-argument argument)
                (doseq [subargument (get @subarguments argument #{})]
                  (when (structure-counter-argument? counter-argument argument subargument)
                    (swap! counter-arguments assoc-in [argument counter-argument] subargument))))))
        
        ;; Build defeater relationships
        defeaters (atom {})]
    
    (doseq [[argument counter-arg-map] @counter-arguments]
      (doseq [[counter-argument disagreement] counter-arg-map]
        (cond
          (structure-preferable? counter-argument disagreement)
          (swap! defeaters assoc-in [argument (:proper defeater-type) counter-argument] disagreement)
          
          (not (structure-preferable? disagreement counter-argument))
          (swap! defeaters assoc-in [argument (:blocking defeater-type) counter-argument] disagreement))))
    
    @defeaters))

;; ============================================================================
;; Warrant Computation - corresponds to Python Interpreter._get_warrant (lines 426-441)
;; ============================================================================

(defn interpreter-get-warrant
  "Get warrant for literal if it can be defended.
   Corresponds to Python Interpreter._get_warrant method (lines 426-441)."
  [interpreter literal mode]
  (let [derivations (interpreter-get-derivations interpreter literal mode)]
    (when (seq derivations)
      (let [defeaters (interpreter-get-defeaters interpreter)]
        (loop [derivations-list (seq derivations)]
          (when (seq derivations-list)  ; Use (seq ...) to properly check for empty
            (let [derivation (first derivations-list)
                  structure (derivation-get-structure derivation)
                  tree (dialectical-tree-create structure defeaters)
                  mark (dialectical-tree-mark tree)]
              (if (= mark (:undefeated mark-type))
                (:argument structure)
                (recur (rest derivations-list))))))))))

;; ============================================================================
;; Proper Query Implementation - corresponds to Python Interpreter.query (lines 399-425)
;; ============================================================================

(defn interpreter-query
  "Full query implementation with dialectical argumentation.
   Corresponds to Python Interpreter.query method (lines 399-425)."
  [interpreter literal mode]
  ;; Try to get warrant directly instead of checking literals set first
  (let [warrant (interpreter-get-warrant interpreter literal mode)]
    (if (not (nil? warrant))  ; Check warrant exists (can be empty for basic facts)
      ;; Check if this is a basic fact (empty warrant) or defeasible conclusion
      (let [literals (interpreter-get-literals interpreter mode)]
        (if (contains? literals literal)
          ;; If literal is in program, warrant justifies it (even if empty for facts)
          [1 warrant]
          ;; If literal not in program but has warrant, it's still valid
          [1 warrant]))
      (let [complement (defs/literal-complement literal)
            complement-warrant (interpreter-get-warrant interpreter complement mode)]
        (if (not (nil? complement-warrant))  ; Same check for complement
          [0 complement-warrant] 
          ;; Only return UNKNOWN if we can't find the literal in the program at all
          (let [literals (interpreter-get-literals interpreter mode)]
            (if (or (contains? literals literal) (contains? literals complement))
              [2 nil]  ; UNDECIDED - literal is in program but neither it nor complement can be defended
              [-1 nil])))))))

;; ============================================================================
;; Updated Query with Explanation
;; ============================================================================

(defn query-with-full-explanation
  "Query with full dialectical argumentation and explanation."
  [interpreter literal]
  (let [result (interpreter-query interpreter literal (:defeasible defs/rule-types))
        [answer warrant] result]
    {:literal literal
     :answer (case answer
               -1 "UNKNOWN"
               1 "YES"
               0 "NO"
               2 "UNDECIDED"
               "UNKNOWN")
     :warrant warrant
     :explanation (case answer
                   -1 "Literal not derivable in program"
                   1 "Literal survives dialectical analysis"
                   0 "Literal's complement survives dialectical analysis"
                   2 "Neither literal nor complement can be defended"
                   "Unknown answer type")}))

;; ============================================================================
;; Convenience Aliases for Test Compatibility
;; ============================================================================

(def is-strictly-more-specific structure-strictly-more-specific?)
(def is-counter-argument structure-counter-argument?)
(def is-equi-specific structure-equi-specific?)
(def is-more-salient structure-more-salient?)
(def is-preferable structure-preferable?)
(def is-proper-defeater structure-proper-defeater?)
(def is-blocking-defeater structure-blocking-defeater?)
(def is-defeater structure-defeater?)

;; Create alias for the interpreter-get-derivations function for tests
(def get-derivations-from-interpreter interpreter-get-derivations)

;; Make sure derivation-get-structure is available for tests 
(def create-structure-from-derivation derivation-get-structure)

;; ============================================================================
;; Export Public API
;; ============================================================================

(def public-api
  "Public API for argumentation engine functionality."
  {:create-interpreter create-interpreter
   :simple-query simple-query
   :interpreter-get-structures interpreter-get-structures
   :interpreter-contradictory? interpreter-contradictory?
   :query-with-basic-explanation query-with-basic-explanation
   :analyze-basic-argumentation analyze-basic-argumentation
   :answer-type answer-type
   :defeater-type defeater-type
   :mark-type mark-type
   :create-structure create-structure
   :create-derivation create-derivation
   :structure-strict? structure-strict?
   :structure-subargument? structure-subargument?
   :structure-strictly-more-specific? structure-strictly-more-specific?
   :structure-counter-argument? structure-counter-argument?
   :structure-equi-specific? structure-equi-specific?
   :structure-more-salient? structure-more-salient?
   :structure-preferable? structure-preferable?
   :structure-proper-defeater? structure-proper-defeater?
   :structure-blocking-defeater? structure-blocking-defeater?
   :structure-defeater? structure-defeater?
   :literals-disagree? literals-disagree?
   :create-index create-index
   :get-derivations get-derivations-from-interpreter})