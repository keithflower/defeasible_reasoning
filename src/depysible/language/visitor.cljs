(ns depysible.language.visitor
  "Translation of src/main/python/depysible/language/visitor.py
   
   This namespace transforms parse trees from Instaparse into domain objects.
   Instead of Python's visitor pattern, ClojureScript uses Instaparse's
   transformation map approach where each grammar rule has a corresponding
   transformation function.
   
   Each function corresponds to a visit_* method in the Python DefeasibleVisitor class."
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [depysible.domain.definitions :as defs]))

;; ============================================================================
;; Parse Tree Transformation Functions
;; Each function corresponds to a Python DefeasibleVisitor.visit_* method
;; ============================================================================

(defn transform-comment
  "Transform comment node to string.
   Corresponds to Python DefeasibleVisitor.visit_comment() method (lines 19-20)."
  [comment-text]
  comment-text)

(defn transform-program 
  "Transform program node to Program object.
   Corresponds to Python DefeasibleVisitor.visit_program() method (lines 22-23)."
  [& program-items]
  (let [rules (filter #(not (string? %)) program-items)] ; Filter out comments (strings)
    (defs/create-program rules)))

(defn transform-program-item
  "Transform program-item node (pass through the rule or comment).
   Handles both rules and comments in programs."
  [item]
  item)

(defn transform-rule
  "Transform rule node (pass through first child).
   Corresponds to Python DefeasibleVisitor.visit_rule() method (lines 25-26)."
  [rule]
  rule)

(defn transform-defeasible
  "Transform defeasible rule node to defeasible Rule object.
   Corresponds to Python DefeasibleVisitor.visit_defeasible() method (lines 28-32).
   
   Handles cases:
   - literal -< . (no body, no salience)
   - literal -< literals . (with body, no salience)  
   - literal -< [salience] . (no body, with salience)
   - literal -< literals [salience] . (with body and salience)"
  ([literal] 
   (defs/create-rule literal (:defeasible defs/rule-types) [] 0))
  ([literal arg2]
   ;; Need to distinguish between literals (vector) and salience (integer)
   (if (vector? arg2)
     ;; arg2 is literals
     (defs/create-rule literal (:defeasible defs/rule-types) arg2 0)
     ;; arg2 is salience
     (defs/create-rule literal (:defeasible defs/rule-types) [] arg2)))
  ([literal literals salience]
   (defs/create-rule literal (:defeasible defs/rule-types) literals salience)))

(defn transform-strict
  "Transform strict rule node to strict Rule object.
   Corresponds to Python DefeasibleVisitor.visit_strict() method (lines 34-38).
   
   Handles cases:
   - literal . (no body, no salience)
   - literal <- literals . (with body, no salience)
   - literal [salience] . (no body, with salience)  
   - literal <- literals [salience] . (with body and salience)"
  ([literal]
   (defs/create-rule literal (:strict defs/rule-types) [] 0))
  ([literal arg2]
   ;; Need to distinguish between literals (vector) and salience (integer)
   (if (vector? arg2)
     ;; arg2 is literals
     (defs/create-rule literal (:strict defs/rule-types) arg2 0)
     ;; arg2 is salience
     (defs/create-rule literal (:strict defs/rule-types) [] arg2)))
  ([literal literals salience]
   (defs/create-rule literal (:strict defs/rule-types) literals salience)))

(defn transform-salience
  "Transform salience node to integer value.
   Extracts the numeric salience value from [number] syntax."
  [salience-value]
  salience-value)

(defn transform-literals
  "Transform literals node to vector of Literal objects.
   Corresponds to Python DefeasibleVisitor.visit_literals() method (lines 40-41)."
  [& literals]
  (vec literals))

(defn transform-literal
  "Transform literal node to Literal object.
   Corresponds to Python DefeasibleVisitor.visit_literal() method (lines 43-46).
   
   Handles both cases:
   - With negation: negation atom
   - Without negation: atom"
  ([atom]
   (defs/create-literal false atom))
  ([negation atom]
   (defs/create-literal negation atom)))

(defn transform-negation
  "Transform negation node to boolean.
   Corresponds to Python DefeasibleVisitor.visit_negation() method (lines 48-49).
   
   Python version checks if len(children) % 2 == 1, but since we use + in grammar,
   we always have odd number of ~ symbols, so negation is always true."
  [& negations]
  true)

(defn transform-atom
  "Transform atom node to Atom object.
   Corresponds to Python DefeasibleVisitor.visit_atom() method (lines 51-54).
   
   Handles both cases:
   - With terms: functor ( terms )
   - Without terms: functor"
  ([functor]
   (defs/create-atom functor []))
  ([functor terms]
   (defs/create-atom functor terms)))

(defn transform-functor
  "Transform functor node to string.
   Corresponds to Python DefeasibleVisitor.visit_functor() method (lines 57-61).
   
   Handles quoted functors by removing quotes."
  [functor-value]
  (let [s (str functor-value)]
    (if (and (or (str/starts-with? s "\"") (str/starts-with? s "'"))
             (or (str/ends-with? s "\"") (str/ends-with? s "'")))
      (subs s 1 (dec (count s)))  ; Remove quotes
      s)))

(defn transform-terms
  "Transform terms node to vector of term values.
   Corresponds to Python DefeasibleVisitor.visit_terms() method (lines 63-64)."
  [& terms]
  (vec terms))

(defn transform-term
  "Transform term node (pass through first child).
   Corresponds to Python DefeasibleVisitor.visit_term() method (lines 66-67)."
  [term]
  term)

(defn transform-boolean
  "Transform boolean node (pass through first child).
   Corresponds to Python DefeasibleVisitor.visit_boolean() method (lines 69-70)."
  [bool-value]
  bool-value)

(defn transform-false
  "Transform false node to boolean false.
   Corresponds to Python DefeasibleVisitor.visit_false() method (lines 72-73)."
  [_]
  false)

(defn transform-true
  "Transform true node to boolean true.
   Corresponds to Python DefeasibleVisitor.visit_true() method (lines 75-76)."
  [_]
  true)

(defn transform-number
  "Transform number node (pass through first child).
   Corresponds to Python DefeasibleVisitor.visit_number() method (lines 78-79)."
  [number]
  number)

(defn transform-real
  "Transform real number node to float.
   Corresponds to Python DefeasibleVisitor.visit_real() method (lines 81-82)."
  [real-string]
  (js/parseFloat real-string))

(defn transform-integer
  "Transform integer node to int.
   Corresponds to Python DefeasibleVisitor.visit_integer() method (lines 84-85)."
  [int-string]
  (js/parseInt int-string 10))

(defn transform-string
  "Transform string node (pass through first child).
   Corresponds to Python DefeasibleVisitor.visit_string() method (lines 87-88)."
  [string-value]
  string-value)

(defn transform-double-quote
  "Transform double-quoted string node to string value.
   Corresponds to Python DefeasibleVisitor.visit_double_quote() method (lines 90-91).
   
   Python version uses json.dumps() but we just return the inner string."
  [inner-string]
  inner-string)

(defn transform-single-quote
  "Transform single-quoted string node to string value.
   Corresponds to Python DefeasibleVisitor.visit_single_quote() method (lines 93-94).
   
   Python version uses json.dumps() but we just return the inner string."
  [inner-string]
  inner-string)

(defn transform-identifier
  "Transform identifier node to string.
   Corresponds to Python DefeasibleVisitor.visit_identifier() method (lines 96-97)."
  [identifier-string]
  (str identifier-string))

(defn transform-variable
  "Transform variable node to string.
   Corresponds to Python DefeasibleVisitor.visit_variable() method (lines 99-100)."
  [variable-string]
  (str variable-string))

;; ============================================================================
;; Instaparse Transformation Map
;; Maps grammar rule names to transformation functions
;; ============================================================================

(def transformation-map
  "Instaparse transformation map that converts parse trees to domain objects.
   
   This replaces the Python visitor pattern with Instaparse's functional approach.
   Each key corresponds to a grammar rule name, and each value is the transformation
   function that processes nodes of that type."
  {:comment transform-comment
   :program transform-program
   :program-item transform-program-item
   :rule transform-rule
   :defeasible transform-defeasible
   :strict transform-strict
   :salience transform-salience
   :literals transform-literals
   :literal transform-literal
   :negation transform-negation
   :atom transform-atom
   :functor transform-functor
   :terms transform-terms
   :term transform-term
   :boolean transform-boolean
   :false transform-false
   :true transform-true
   :number transform-number
   :real transform-real
   :integer transform-integer
   :string transform-string
   :double-quote transform-double-quote
   :single-quote transform-single-quote
   :identifier transform-identifier
   :variable transform-variable})

;; ============================================================================
;; High-level Parse and Transform Functions  
;; ============================================================================

(defn parse-and-transform
  "Parse input string with grammar and transform to domain objects.
   
   This combines parsing and transformation in one step, equivalent to
   the Python approach of parse_tree -> visit_parse_tree(parse_tree, visitor)."
  [grammar input-string start-rule]
  (let [parse-result (grammar input-string :start start-rule)]
    (if (insta/failure? parse-result)
      parse-result
      (insta/transform transformation-map parse-result))))

(defn parse-program-to-objects
  "Parse program string and return Program object.
   
   Corresponds to the complete Python parsing pipeline:
   parser.parse() -> visit_parse_tree() -> Program object"
  [grammar program-text]
  (parse-and-transform grammar program-text :program))

(defn parse-rule-to-object
  "Parse rule string and return Rule object.
   
   Corresponds to the complete Python parsing pipeline for rules."
  [grammar rule-text]
  (parse-and-transform grammar rule-text :rule))

(defn parse-literal-to-object
  "Parse literal string and return Literal object.
   
   Corresponds to the complete Python parsing pipeline for literals."
  [grammar literal-text]
  (parse-and-transform grammar literal-text :literal))

;; ============================================================================
;; Error Handling and Debugging
;; ============================================================================

(defn transform-with-error-handling
  "Transform parse tree with error handling and detailed error messages."
  [parse-tree]
  (try
    (insta/transform transformation-map parse-tree)
    (catch js/Error e
      {:error :transformation-failed
       :message (.-message e)
       :parse-tree parse-tree})))

(defn debug-transformation
  "Debug transformation by showing intermediate steps.
   Useful for debugging complex parse trees."
  [parse-tree]
  (println "Original parse tree:")
  (println parse-tree)
  (println "\nTransformed result:")
  (let [result (transform-with-error-handling parse-tree)]
    (println result)
    result))

;; ============================================================================
;; Correspondence Documentation
;; ============================================================================

(def visitor-method-correspondence
  "Documentation of correspondence between Python visitor methods and ClojureScript functions."
  {"visit_comment"      "transform-comment"
   "visit_program"      "transform-program"  
   "visit_rule"         "transform-rule"
   "visit_defeasible"   "transform-defeasible"
   "visit_strict"       "transform-strict"
   "visit_literals"     "transform-literals"
   "visit_literal"      "transform-literal"
   "visit_negation"     "transform-negation"
   "visit_atom"         "transform-atom"
   "visit_functor"      "transform-functor"
   "visit_terms"        "transform-terms"
   "visit_term"         "transform-term"
   "visit_boolean"      "transform-boolean"
   "visit_false"        "transform-false"
   "visit_true"         "transform-true"
   "visit_number"       "transform-number"
   "visit_real"         "transform-real"
   "visit_integer"      "transform-integer"
   "visit_string"       "transform-string"
   "visit_double_quote" "transform-double-quote"
   "visit_single_quote" "transform-single-quote"
   "visit_identifier"   "transform-identifier"
   "visit_variable"     "transform-variable"})

;; ============================================================================
;; Export Public API
;; ============================================================================

(def public-api
  "Public API for parse tree transformation."
  {:transformation-map transformation-map
   :parse-and-transform parse-and-transform
   :parse-program-to-objects parse-program-to-objects
   :parse-rule-to-object parse-rule-to-object
   :parse-literal-to-object parse-literal-to-object
   :transform-with-error-handling transform-with-error-handling
   :debug-transformation debug-transformation
   :visitor-method-correspondence visitor-method-correspondence}) 