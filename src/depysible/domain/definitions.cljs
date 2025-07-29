(ns depysible.domain.definitions
  "Translation of src/main/python/depysible/domain/definitions.py
   
   This namespace contains the core data structures for the defeasible logic system:
   - Atom: Represents atomic propositions with functor and terms
   - Literal: Represents atoms with negation
   - Rule: Represents strict and defeasible rules  
   - Program: Represents collections of rules
   
   All data structures maintain the same semantics as the Python originals
   but use immutable ClojureScript data structures and functional patterns."
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; ============================================================================
;; Type Aliases (Python lines 12-15)
;; Original Python:
;;   Value = Union[bool, int, float, str]
;;   Variable = str
;;   Term = Union[Value, Variable]
;;   Substitutions = Dict[Variable, Term]
;; ============================================================================

(defn value? 
  "Check if term is a value (bool, int, float, string).
   Corresponds to Python Value type."
  [term]
  (or (boolean? term) (number? term) (string? term)))

(defn variable?
  "Check if term is a variable (starts with uppercase or underscore).
   Corresponds to Python Variable type and Atom.is_variable() method (line 68-69)."
  [term]
  (and (string? term)
       (re-matches #"[_A-Z][a-zA-Z_0-9]*" term)))

(defn term? 
  "Check if term is valid (either value or variable).
   Corresponds to Python Term type."
  [term]
  (or (value? term) (variable? term)))

;; Substitutions represented as ClojureScript maps

;; ============================================================================
;; Forward Declarations for Functions Used in Record Methods
;; ============================================================================

(declare atom->string)
(declare literal->string)
(declare rule->string)
(declare program->string)
(declare create-rule)
(declare program-get-ground-program)
(declare rule-as-literals)
(declare rule-ground?)
(declare rule-presumption?)

;; ============================================================================
;; Helper Functions for Comparison
;; ============================================================================

(defn type-order
  "Get the sort order for different types."
  [x]
  (cond
    (boolean? x) 0
    (number? x) 1
    (string? x) 2
    :else 3))

(defn compare-mixed
  "Compare two values that might be of different types."
  [x y]
  (let [x-type (type-order x)
        y-type (type-order y)]
    (if (= x-type y-type)
      (compare x y)
      (compare x-type y-type))))

(defn compare-terms
  "Compare two term vectors lexicographically."
  [terms1 terms2]
  (loop [t1 terms1 t2 terms2]
    (cond
      (and (empty? t1) (empty? t2)) 0
      (empty? t1) -1
      (empty? t2) 1
      :else
      (let [result (compare-mixed (first t1) (first t2))]
        (if (zero? result)
          (recur (rest t1) (rest t2))
          result)))))

;; ============================================================================
;; Atom Record (Python lines 18-69)
;; Original Python class Atom with functor and terms fields
;; ============================================================================

(defn atom->string
  "Convert atom to string representation.
   Corresponds to Python Atom.__repr__ method (lines 26-31)."
  [{:keys [functor terms]}]
  (if (empty? terms)
    functor
    (str functor "(" (str/join ", " (map str terms)) ")")))

(defrecord LogicAtom [functor terms]
  Object
  (toString [this] (atom->string this))
  
  IComparable
  (-compare [this other]
    (when (instance? LogicAtom other)
      (let [functor-cmp (compare (:functor this) (:functor other))]
        (if (zero? functor-cmp)
          (let [arity-cmp (compare (count (:terms this)) (count (:terms other)))]
            (if (zero? arity-cmp)
              (compare-terms (:terms this) (:terms other))
              arity-cmp))
          functor-cmp)))))

(defn create-atom
  "Create a new Atom with functor and terms.
   Corresponds to Python Atom constructor."
  [functor terms]
  ;; Input validation
  (when (or (nil? functor) 
            (and (string? functor) (or (empty? functor) (str/blank? functor))))
    (throw (js/Error. "Functor cannot be nil, empty, or whitespace-only")))
  (when (nil? terms)
    (throw (js/Error. "Terms cannot be nil")))
  (->LogicAtom functor (vec terms)))

(defn atom-arity
  "Return the arity (number of terms) of an atom.
   Corresponds to Python Atom.arity() method (lines 32-33)."
  [atom]
  (count (:terms atom)))

(defn atom-ground?
  "Check if atom is ground (contains no variables).
   Corresponds to Python Atom.is_ground() method (lines 35-41)."
  [atom]
  (not-any? variable? (:terms atom)))

(defn atom-unifies
  "Attempt to unify this atom with a ground atom, returning substitutions.
   Corresponds to Python Atom.unifies() method (lines 42-63).
   
   Returns nil if unification fails, otherwise returns substitution map."
  [atom ground-atom]
  (when (and (instance? LogicAtom ground-atom)
             (= (:functor atom) (:functor ground-atom))
             (= (atom-arity atom) (atom-arity ground-atom)))
    (loop [substitutions {}
           terms (:terms atom)
           ground-terms (:terms ground-atom)]
      (if (empty? terms)
        substitutions  ; Success - return accumulated substitutions
        (let [term (first terms)
              ground-term (first ground-terms)]
          (cond
            ;; If term is a variable
            (variable? term)
            (let [existing-binding (get substitutions term)]
              (cond
                ;; Variable not yet bound - bind it
                (nil? existing-binding)
                (recur (assoc substitutions term ground-term)
                       (rest terms) 
                       (rest ground-terms))
                
                ;; Variable already bound to same value - continue
                (= existing-binding ground-term)
                (recur substitutions (rest terms) (rest ground-terms))
                
                ;; Variable bound to different value - conflict!
                :else
                nil))
            
            ;; If term is not a variable, it must match exactly
            (= term ground-term)
            (recur substitutions (rest terms) (rest ground-terms))
            
            ;; Ground terms don't match - failure
            :else
            nil))))))

(defn atom-substitute
  "Apply substitutions to atom, replacing variables with their values.
   Corresponds to Python Atom.substitutes() method (lines 65-66)."
  [atom substitutions]
  (let [new-terms (mapv (fn [term]
                          (if (variable? term)
                            (get substitutions term "_")
                            term))
                        (:terms atom))]
    (create-atom (:functor atom) new-terms)))

;; ============================================================================
;; Rule Type Enumeration (Python lines 125-127)
;; Original Python: class RuleType(Enum): STRICT = 0, DEFEASIBLE = 1
;; ============================================================================

(def rule-types
  "Rule type constants corresponding to Python RuleType enum."
  {:strict 0
   :defeasible 1})

(defn strict-rule? [rule-type] (= rule-type (:strict rule-types)))
(defn defeasible-rule? [rule-type] (= rule-type (:defeasible rule-types)))

;; ============================================================================
;; Literal Record (Python lines 72-123)
;; Original Python class Literal with negated and atom fields
;; ============================================================================

(defrecord Literal [negated atom]
  Object
  (toString [this] (literal->string this))
  
  IComparable
  (-compare [this other]
    (when (instance? Literal other)
      (let [negation-cmp (compare (:negated this) (:negated other))]
        (if (zero? negation-cmp)
          (compare (:atom this) (:atom other))
          negation-cmp)))))

(defn create-literal
  "Create a new Literal with negation flag and atom.
   Corresponds to Python Literal constructor."
  [negated atom]
  ;; Input validation
  (when (nil? atom)
    (throw (js/Error. "Atom cannot be nil")))
  (->Literal negated atom))

(defn literal->string
  "Convert literal to string representation.
   Corresponds to Python Literal.__repr__ method (lines 93-94)."
  [{:keys [negated atom]}]
  (str (when negated "~") (atom->string atom)))

(defn literal-functor
  "Get the functor of the literal's atom.
   Corresponds to Python Literal.functor property (lines 96-98)."
  [literal]
  (:functor (:atom literal)))

(defn literal-terms
  "Get the terms of the literal's atom.
   Corresponds to Python Literal.terms property (lines 100-102)."
  [literal]
  (:terms (:atom literal)))

(defn literal-arity
  "Get the arity of the literal's atom.
   Corresponds to Python Literal.arity() method (lines 104-105)."
  [literal]
  (atom-arity (:atom literal)))

(defn literal-ground?
  "Check if literal is ground (atom contains no variables).
   Corresponds to Python Literal.is_ground() method (lines 107-108)."
  [literal]
  (atom-ground? (:atom literal)))

(defn literal-unifies
  "Attempt to unify this literal with a ground literal.
   Corresponds to Python Literal.unifies() method (lines 110-117).
   
   Returns nil if unification fails (different types or negation),
   otherwise returns substitution map from atom unification."
  [literal ground-literal]
  (when (and (instance? Literal ground-literal)
             (= (:negated literal) (:negated ground-literal)))
    (atom-unifies (:atom literal) (:atom ground-literal))))

(defn literal-substitute
  "Apply substitutions to literal.
   Corresponds to Python Literal.substitutes() method (lines 119-120)."
  [literal substitutions]
  (create-literal (:negated literal)
                  (atom-substitute (:atom literal) substitutions)))

(defn literal-complement
  "Get the complement of a literal (flip negation).
   Corresponds to Python Literal.get_complement() method (lines 122-123)."
  [literal]
  (create-literal (not (:negated literal)) (:atom literal)))

(defn literal-as-fact
  "Convert literal to a strict rule with no body.
   Corresponds to Python Literal.as_fact() method (lines 125-126)."
  [literal]
  (create-rule literal (:strict rule-types) [] 0))

;; ============================================================================
;; Rule Record (Python lines 130-190)
;; Original Python class Rule with head, type, body, and salience fields
;; ============================================================================

(defrecord Rule [head type body salience]
  Object
  (toString [this] (rule->string this)))

(defn create-rule
  "Create a new Rule.
   Corresponds to Python Rule constructor with default salience of 0."
  ([head type body] (create-rule head type body 0))
  ([head type body salience]
   ;; Input validation
   (when (nil? head)
     (throw (js/Error. "Head cannot be nil")))
   (->Rule head type body salience)))

(defn rule->string
  "Convert rule to string representation.
   Corresponds to Python Rule.__repr__ method (lines 152-159)."
  [{:keys [head type body]}]
  (let [head-str (literal->string head)
        arrow (if (strict-rule? type) " <- " " -< ")
        body-str (when (seq body) (str/join ", " (map literal->string body)))
        has-arrow? (or (seq body) (defeasible-rule? type))]
    (str head-str
         (when has-arrow? arrow)
         (or body-str "")
         ".")))

(defn rule-fact?
  "Check if rule is a fact (strict rule with no body).
   Corresponds to Python Rule.is_fact() method (lines 161-162)."
  [rule]
  (and (strict-rule? (:type rule))
       (empty? (:body rule))))

(defn rule-presumption?
  "Check if rule is a presumption (defeasible rule with no body).
   Corresponds to Python Rule.is_presumption() method (lines 164-165)."
  [rule]
  (and (defeasible-rule? (:type rule))
       (empty? (:body rule))))

(defn rule-ground?
  "Check if rule is ground (all literals are ground).
   Corresponds to Python Rule.is_ground() method (lines 167-168)."
  [rule]
  (and (literal-ground? (:head rule))
       (every? literal-ground? (:body rule))))

(defn rule-as-literals
  "Get all literals from rule (head + body).
   Corresponds to Python Rule.as_literals() method (lines 170-171)."
  [rule]
  (concat [(:head rule)] (:body rule)))

;; ============================================================================
;; Program Record (Python lines 178-305)
;; Original Python class Program with rules and caching fields
;; ============================================================================

(defrecord Program [rules ground-cache strict-cache defeasible-cache]
  Object
  (toString [this] (program->string this)))

(defn create-program
  "Create a new Program with rules.
   Corresponds to Python Program constructor."
  [rules]
  (->Program (vec rules) nil nil nil))

(defn program->string
  "Convert program to formatted string representation.
   Corresponds to Python Program.__repr__ method (lines 212-249).
   
   Groups rules by type and formats them in sections."
  [program]
  (let [rules (:rules program)
        strict-rules (filter #(and (strict-rule? (:type %))
                                   (not (rule-fact? %))) rules)
        facts (filter rule-fact? rules)
        defeasible-rules (filter #(and (defeasible-rule? (:type %))
                                       (not (rule-presumption? %))) rules)
        presumptions (filter rule-presumption? rules)
        
        format-section (fn [title rules]
                         (when (seq rules)
                           (str "# " title "\n"
                                (str/join "\n" (map rule->string (sort-by rule->string rules))))))
        
        sections (remove nil? [(format-section "Strict rules" strict-rules)
                              (format-section "Facts" facts)
                              (format-section "Defeasible knowledge" 
                                             (concat defeasible-rules presumptions))])]
    (str/join "\n\n" sections)))

(defn program-get-facts
  "Get all facts (strict rules with no body) from program.
   Corresponds to Python Program.get_facts() method (lines 251-252)."
  [program]
  (filter rule-fact? (:rules program)))

(defn program-get-presumptions
  "Get all presumptions (defeasible rules with no body) from program.
   Corresponds to Python Program.get_presumptions() method (lines 254-255)."
  [program]
  (filter rule-presumption? (:rules program)))

(defn program-get-rules
  "Get rules of specified type, excluding facts and presumptions.
   Corresponds to Python Program.get_rules() method (lines 257-259)."
  ([program] (program-get-rules program nil))
  ([program rule-type]
   (filter (fn [rule]
             (and (not (rule-fact? rule))
                  (not (rule-presumption? rule))
                  (or (nil? rule-type) (= (:type rule) rule-type))))
           (:rules program))))

(defn program-get-strict
  "Get all strict rules from ground program.
   Corresponds to Python Program.get_strict() method (lines 261-272)."
  [program]
  (let [ground-prog (program-get-ground-program program)]
    (when-not (:strict-cache ground-prog)
      ;; Cache computation similar to Python version
      (let [strict-rules (set (filter #(strict-rule? (:type %)) (:rules ground-prog)))
            defeasible-rules (set (filter #(defeasible-rule? (:type %)) (:rules ground-prog)))]
        ;; Update the ground program with cached values
        (assoc ground-prog 
               :strict-cache strict-rules
               :defeasible-cache defeasible-rules)))
    (:strict-cache ground-prog)))

(defn program-get-defeasible
  "Get all defeasible rules from ground program.
   Corresponds to Python Program.get_defeasible() method (lines 274-284)."
  [program]
  (let [ground-prog (program-get-ground-program program)]
    (when-not (:defeasible-cache ground-prog)
      ;; Cache computation similar to Python version
      (let [strict-rules (set (filter #(strict-rule? (:type %)) (:rules ground-prog)))
            defeasible-rules (set (filter #(defeasible-rule? (:type %)) (:rules ground-prog)))]
        ;; Update the ground program with cached values
        (assoc ground-prog
               :strict-cache strict-rules
               :defeasible-cache defeasible-rules)))
    (:defeasible-cache ground-prog)))

(defn program-as-literals
  "Get all unique literals from all rules in program.
   Corresponds to Python Program.as_literals() method (lines 286-287)."
  [program]
  (set (mapcat rule-as-literals (:rules program))))

(defn program-ground?
  "Check if program is ground (all rules are ground).
   Corresponds to Python Program.is_ground() method (lines 289-294)."
  [program]
  (every? rule-ground? (:rules program)))

(defn program-get-ground-program
  "Get ground version of program using RETE algorithm.
   Corresponds to Python Program.get_ground_program() method (lines 296-305).
   
   Note: This requires the RETE module for rule firing. The circular dependency
   is resolved by requiring rete in the calling namespace."
  [program]
  (if (program-ground? program)
    program
    (or (:ground-cache program)
        ;; This will be called by external namespaces that require both 
        ;; definitions and rete modules
        (throw (js/Error. "Ground program generation requires RETE algorithm - use depysible.core functions")))))

;; ============================================================================
;; String and Object Protocol Implementations
;; ============================================================================

;; Extend toString for clean printing
(extend-protocol IPrintWithWriter
  LogicAtom
  (-pr-writer [atom writer _]
    (write-all writer (atom->string atom)))
  
  Literal  
  (-pr-writer [literal writer _]
    (write-all writer (literal->string literal)))
    
  Rule
  (-pr-writer [rule writer _]
    (write-all writer (rule->string rule)))
    
  Program
  (-pr-writer [program writer _]
    (write-all writer (program->string program))))

;; ============================================================================
;; Parse Functions (Forward References)
;; ============================================================================

;; These will be implemented once the grammar and visitor modules are translated
;; Corresponds to Python static parse methods in Literal, Rule, and Program classes

(declare parse-literal)   ; Will be implemented in grammar namespace
(declare parse-rule)      ; Will be implemented in grammar namespace  
(declare parse-program)   ; Will be implemented in grammar namespace

;; ============================================================================
;; Export Public API
;; ============================================================================

;; All functions are automatically exported by ClojureScript namespace system
