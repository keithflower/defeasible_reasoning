(ns depysible.language.grammar
  "Translation of src/main/python/depysible/language/grammar.py
   
   This namespace defines the grammar for parsing defeasible logic programs
   using Instaparse instead of Python's Arpeggio library.
   
   The grammar structure remains identical to the Python version but uses
   Instaparse's EBNF syntax instead of Arpeggio's functional definitions."
  (:require [instaparse.core :as insta]))

;; ============================================================================
;; Grammar Definition using Instaparse
;; Corresponds to Python grammar functions (lines 1-102)
;; ============================================================================

(def defeasible-grammar
  "Instaparse grammar for defeasible logic programs.
   
   This translates the Python Arpeggio grammar functions to Instaparse EBNF:
   - nothing()     -> (not needed in Instaparse)
   - comment()     -> comment rule (line 12-13)
   - program()     -> program rule (line 16-17) 
   - rule()        -> rule rule (line 20-21)
   - defeasible()  -> defeasible rule (line 24-25)
   - strict()      -> strict rule (line 28-29)
   - literals()    -> literals rule (line 32-33)
   - literal()     -> literal rule (line 36-37)
   - negation()    -> negation rule (line 40-41)
   - atom()        -> atom rule (line 44-45)
   - functor()     -> functor rule (line 48-49)
   - terms()       -> terms rule (line 52-53)
   - term()        -> term rule (line 56-57)
   - boolean()     -> boolean rule (line 60-61)
   - false()       -> false rule (line 64-65)
   - true()        -> true rule (line 68-69)
   - number()      -> number rule (line 72-73)
   - real()        -> real rule (line 76-77)
   - integer()     -> integer rule (line 80-81)
   - string()      -> string rule (line 84-85)
   - double_quote() -> double-quote rule (line 88-89)
   - single_quote() -> single-quote rule (line 92-93)
   - identifier()  -> identifier rule (line 96-97)
   - variable()    -> variable rule (line 100-101)"
  
  (insta/parser
    "
    (* Top-level program structure - corresponds to Python program() function *)
    program = program-item*
    program-item = <whitespace>* (rule | comment) <whitespace>*
    
    (* Comments - corresponds to Python comment() function *)
    comment = #'%.*'
    
    (* Rule types - corresponds to Python rule() function *)
    rule = defeasible | strict
    
    (* Defeasible rule - corresponds to Python defeasible() function *)
    defeasible = <#'\\s*'> literal <#'\\s*-<\\s*'> literals? <#'\\s*'> salience? <#'\\s*'> <'.'> <#'\\s*'>
    
    (* Strict rule - corresponds to Python strict() function *)  
    strict = <#'\\s*'> literal (<#'\\s*<-\\s*'> literals?)? <#'\\s*'> salience? <#'\\s*'> <'.'> <#'\\s*'>
    
    (* Salience annotation - optional numeric priority *)
    salience = <#'\\s*'> <'['> <#'\\s*'> integer <#'\\s*'> <']'> <#'\\s*'>
    
    (* List of literals - corresponds to Python literals() function *)
    literals = literal (<#'\\s*,\\s*'> literal)*
    
    (* Literal with optional negation - corresponds to Python literal() function *)
    literal = negation? <#'\\s*'> atom
    
    (* Negation operator - corresponds to Python negation() function *)
    negation = <'~'>+
    
    (* Atom with functor and optional terms - corresponds to Python atom() function *)
    atom = functor (<#'\\s*'> <'('> <#'\\s*'> terms? <#'\\s*'> <')'>)?
    
    (* Functor can be string or identifier - corresponds to Python functor() function *)
    functor = double-quote | single-quote | identifier
    
    (* List of terms - corresponds to Python terms() function *)  
    terms = <#'\\s*'> term (<#'\\s*,\\s*'> term)* <#'\\s*'>
    
    (* Term types - corresponds to Python term() function *)
    term = boolean | number | string | variable | identifier
    
    (* Boolean values - corresponds to Python boolean() function *)
    boolean = false | true
    
       (* False literal - corresponds to Python false() function *)
   false = #'(?i)(false|FALSE)'
   
   (* True literal - corresponds to Python true() function *)
   true = #'(?i)(true|TRUE)'  
    
    (* Number types - corresponds to Python number() function *)
    number = real | integer
    
    (* Real number - corresponds to Python real() function *)
    real = #'-?\\d*\\.\\d+(E-?\\d+)?'
    
    (* Integer - corresponds to Python integer() function *)
    integer = #'-?\\d+'
    
    (* String types - corresponds to Python string() function *)
    string = double-quote | single-quote
    
    (* Double-quoted string - corresponds to Python double_quote() function *)
    double-quote = <'\"'> #'[^\"]*' <'\"'>
    
    (* Single-quoted string - corresponds to Python single_quote() function *)
    single-quote = <\"'\"  > #\"[^']*\" <\"'\">
    
    (* Identifier (lowercase start) - corresponds to Python identifier() function *)
    identifier = !boolean #'[a-z][a-zA-Z_0-9]*'
    
    (* Variable (uppercase/underscore start) - corresponds to Python variable() function *)
    variable = !boolean #'[_A-Z][a-zA-Z_0-9]*'
    
    (* Whitespace handling *)
    <whitespace> = #'\\s+'
    "))

;; ============================================================================
;; Parser Functions
;; Corresponds to the parsing functionality from the Python version
;; ============================================================================

(defn parse-program
  "Parse a complete defeasible logic program from string.
   Corresponds to Python Program.parse() static method in definitions.py."
  [program-text]
  (defeasible-grammar program-text :start :program))

(defn parse-rule
  "Parse a single rule from string.
   Corresponds to Python Rule.parse() static method in definitions.py."
  [rule-text]
  (defeasible-grammar rule-text :start :rule))

(defn parse-literal
  "Parse a single literal from string.
   Corresponds to Python Literal.parse() static method in definitions.py."
  [literal-text]
  (defeasible-grammar literal-text :start :literal))

(defn parse-atom
  "Parse a single atom from string.
   Helper function for parsing atoms independently."
  [atom-text]
  (defeasible-grammar atom-text :start :atom))

(defn parse-term
  "Parse a single term from string.
   Helper function for parsing terms independently."
  [term-text] 
  (defeasible-grammar term-text :start :term))

;; ============================================================================
;; Grammar Testing and Validation
;; ============================================================================

(defn valid-parse?
  "Check if parsing was successful (no parse errors).
   Returns true if parse tree is valid, false otherwise."
  [parse-result]
  (not (insta/failure? parse-result)))

(defn parse-error-message
  "Get human-readable error message from failed parse.
   Returns nil if parse was successful."
  [parse-result]
  (when (insta/failure? parse-result)
    (str "Parse error: " (pr-str parse-result))))

;; ============================================================================
;; Example Usage and Testing
;; ============================================================================

(comment
  "Example usage of the grammar parser:
   
   ;; Parse a complete program
   (parse-program \"bird(X) <- chicken(X). chicken(tina).\")
   
   ;; Parse individual rules  
   (parse-rule \"flies(X) -< bird(X).\")
   (parse-rule \"~flies(X) <- penguin(X).\")
   
   ;; Parse literals
   (parse-literal \"bird(tweety)\")
   (parse-literal \"~flies(X)\")
   
   ;; Check for parse errors
   (def result (parse-rule \"invalid syntax\"))
   (if (valid-parse? result)
     (println \"Successfully parsed:\" result)
     (println (parse-error-message result)))")

;; ============================================================================
;; Grammar Rule Correspondence Table
;; ============================================================================

(def grammar-mapping
  "Mapping between Python Arpeggio functions and Instaparse rules.
   This serves as documentation for the translation."
  {;; Python function -> Instaparse rule
   "nothing()"      "Not needed - Instaparse handles EOF automatically"
   "comment()"      "comment = #'%.*'"
   "program()"      "program = rule*"  
   "rule()"         "rule = defeasible | strict"
   "defeasible()"   "<defeasible> = literal <'-<'> literals? <'.'>"
   "strict()"       "<strict> = literal (<'<-'> literals?)? <'.'>"
   "literals()"     "literals = literal (<','> literal)*"
   "literal()"      "literal = negation? atom"
   "negation()"     "negation = <'~'>+"
   "atom()"         "atom = functor (<'('> terms? <')'>)?"
   "functor()"      "functor = double-quote | single-quote | identifier"
   "terms()"        "terms = term (<','> term)*"
   "term()"         "term = boolean | number | string | identifier | variable"
   "boolean()"      "boolean = false | true"
   "false()"        "false = #'(?i)FALSE'"
   "true()"         "true = #'(?i)TRUE'"
   "number()"       "number = real | integer"
   "real()"         "real = #'-?\\\\d*\\\\.\\\\d+(E-?\\\\d+)?'"
   "integer()"      "integer = #'-?\\\\d+'"
   "string()"       "string = double-quote | single-quote"
   "double_quote()" "double-quote = <'\\\"'> #'[^\\\"]*' <'\\\"'>"
   "single_quote()" "single-quote = <\\\"'\\\" > #\\\"[^']*\\\" <\\\"'\\\">"
   "identifier()"   "identifier = #'[a-z][a-zA-Z_0-9]*'"
   "variable()"     "variable = #'[_A-Z][a-zA-Z_0-9]*'"})

;; ============================================================================
;; Export Public API
;; ============================================================================

(def public-api
  "Public functions for grammar parsing."
  {:parse-program parse-program
   :parse-rule parse-rule
   :parse-literal parse-literal
   :parse-atom parse-atom
   :parse-term parse-term
   :valid-parse? valid-parse?
   :parse-error-message parse-error-message
   :grammar defeasible-grammar
   :grammar-mapping grammar-mapping}) 