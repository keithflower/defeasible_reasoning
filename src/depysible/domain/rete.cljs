(ns depysible.domain.rete
  "Translation of src/main/python/depysible/domain/rete.py
   
   This namespace implements the RETE algorithm for efficient pattern matching
   and rule firing in defeasible logic programs. RETE (Latin for 'network') 
   is a pattern matching algorithm that builds a network of nodes to incrementally
   match patterns and fire rules efficiently.
   
   The Python implementation uses classes (Root, Alfa, Beta, Leaf) which are
   translated to ClojureScript records with associated functions."
  (:require [clojure.set :as set]
            [depysible.domain.definitions :as defs]))

;; ============================================================================
;; Forward Declarations
;; ============================================================================

(declare child-notify)
(declare alfa-notify)
(declare beta-notify)
(declare leaf-notify)

;; ============================================================================
;; Type Definitions and Payload Structure
;; Corresponds to Python type hints (lines 1-6)
;; ============================================================================

;; Payload = Tuple[List['Literal'], 'Substitutions'] in Python
;; Represented as [literals substitutions] vector in ClojureScript

(defn create-payload
  "Create a payload tuple containing literals and substitutions.
   Corresponds to Python Payload type definition."
  [literals substitutions]
  [literals substitutions])

(defn payload-literals 
  "Extract literals from payload."
  [payload]
  (first payload))

(defn payload-substitutions
  "Extract substitutions from payload."
  [payload]
  (second payload))

;; ============================================================================
;; Root Node - corresponds to Python Root class (lines 9-16)
;; ============================================================================

(defrecord ReteRoot [children])

(defn create-root
  "Create a RETE root node.
   Corresponds to Python Root.__init__ method (lines 10-11)."
  []
  (->ReteRoot (atom #{})))

(defn root-notify
  "Notify all children of the root with a new ground literal.
   Corresponds to Python Root.notify method (lines 13-15)."
  [root ground-literal]
  (doseq [child @(:children root)]
    (child-notify child ground-literal {} root)))

(defn root-add-child
  "Add a child node to the root."
  [root child]
  (swap! (:children root) conj child))

;; ============================================================================
;; Alfa (Alpha) Node - corresponds to Python Alfa class (lines 19-33)
;; ============================================================================

(defrecord ReteAlfa [pattern parent name memory children])

(defn create-alfa
  "Create a RETE alpha node for single pattern matching.
   Corresponds to Python Alfa.__init__ method (lines 20-27)."
  [pattern parent]
  (let [alfa (->ReteAlfa pattern 
                         parent 
                         (str pattern)  ; Python: repr(pattern)
                         (atom [])      ; memory
                         (atom #{}))]   ; children
    (root-add-child parent alfa)
    alfa))

(defn alfa-notify
  "Process notification in alpha node.
   Corresponds to Python Alfa.notify method (lines 29-35)."
  [alfa ground-literal subs parent]
  (let [unification-result (defs/literal-unifies (:pattern alfa) ground-literal)]
    (when (some? unification-result)
      (let [payload (create-payload [ground-literal] unification-result)]
        (when-not (some #(= % payload) @(:memory alfa))
          (swap! (:memory alfa) conj payload)
          (doseq [child @(:children alfa)]
            (child-notify child [ground-literal] unification-result alfa)))))))

(defn alfa-add-child
  "Add a child node to this alpha node."
  [alfa child]
  (swap! (:children alfa) conj child))

;; ============================================================================
;; Beta Node - corresponds to Python Beta class (lines 36-71)
;; ============================================================================

(defrecord ReteBeta [parent-1 parent-2 name memory children])

(defn create-beta
  "Create a RETE beta node for joining two parent nodes.
   Corresponds to Python Beta.__init__ method (lines 37-44)."
  [parent-1 parent-2]
  (let [name (str (:name parent-1) ", " (:name parent-2))
        beta (->ReteBeta parent-1 
                         parent-2 
                         name
                         (atom [])    ; memory
                         (atom #{}))] ; children
    (alfa-add-child parent-1 beta)
    (alfa-add-child parent-2 beta)
    beta))

(defn beta-unifies-substitutions
  "Check if two substitution maps can be unified.
   Corresponds to Python Beta._unifies static method (lines 53-58)."
  [subs-1 subs-2]
  (let [common-vars (set/intersection (set (keys subs-1)) (set (keys subs-2)))]
    (if (every? #(= (get subs-1 %) (get subs-2 %)) common-vars)
      (merge subs-1 subs-2)  ; Merge if all common variables have same values
      nil)))                  ; Return nil if conflict

(defn beta-internal-notify
  "Internal notification processing for beta nodes.
   Corresponds to Python Beta._notify method (lines 60-68)."
  [beta ground-1 subs-1 ground-2 subs-2]
  (let [unified-subs (beta-unifies-substitutions subs-1 subs-2)]
    (when (some? unified-subs)
      (let [combined-ground (vec (concat ground-1 ground-2))
            payload (create-payload combined-ground unified-subs)]
        (when-not (some #(= % payload) @(:memory beta))
          (swap! (:memory beta) conj payload)
          (doseq [child @(:children beta)]
            (child-notify child combined-ground unified-subs beta)))))))

(defn beta-notify
  "Process notification in beta node.
   Corresponds to Python Beta.notify method (lines 46-51)."
  [beta ground-literals subs parent]
  (cond
    (identical? parent (:parent-1 beta))
    (doseq [[ground-2 subs-2] @(:memory (:parent-2 beta))]
      (beta-internal-notify beta ground-literals subs ground-2 subs-2))
    
    (identical? parent (:parent-2 beta))
    (doseq [[ground-1 subs-1] @(:memory (:parent-1 beta))]
      (beta-internal-notify beta ground-1 subs-1 ground-literals subs))))

(defn beta-add-child
  "Add a child node to this beta node."
  [beta child]
  (swap! (:children beta) conj child))

;; ============================================================================
;; Leaf Node - corresponds to Python Leaf class (lines 74-97)
;; ============================================================================

(defrecord ReteLeaf [rule parent name memory root agenda])

;; ============================================================================
;; Child Notification Dispatch
;; Handles polymorphic notification for different node types
;; ============================================================================

(defn child-notify
  "Generic child notification dispatch function.
   Routes notifications to the appropriate node type handler."
  [child ground-literals subs parent]
  (cond
    (instance? ReteAlfa child) (alfa-notify child ground-literals subs parent)
    (instance? ReteBeta child) (beta-notify child ground-literals subs parent)
    (instance? ReteLeaf child) (leaf-notify child ground-literals subs parent)
    :else (throw (js/Error. (str "Unknown child node type: " (type child))))))

(defn create-leaf
  "Create a RETE leaf node that fires rules.
   Corresponds to Python Leaf.__init__ method (lines 75-83)."
  [rule parent root agenda]
  (let [leaf (->ReteLeaf rule
                         parent
                         (str rule)  ; Python: repr(rule)
                         (atom [])   ; memory
                         root
                         agenda)]
    (cond
      (instance? ReteAlfa parent) (alfa-add-child parent leaf)
      (instance? ReteBeta parent) (beta-add-child parent leaf)
      :else (throw (js/Error. "Invalid parent type for leaf node")))
    leaf))

(defn leaf-notify
  "Process notification in leaf node and fire rule.
   Corresponds to Python Leaf.notify method (lines 85-97)."
  [leaf ground-literals subs parent]
  (let [payload (create-payload ground-literals subs)]
    (when-not (some #(= % payload) @(:memory leaf))
      (swap! (:memory leaf) conj payload)
      
      ;; Apply substitutions to rule head
      (let [substituted-head (defs/literal-substitute (:head (:rule leaf)) subs)]
        ;; Create derived fact preserving original rule type
        ;; - If original rule was strict, create strict fact (empty body)
        ;; - If original rule was defeasible, create defeasible fact (empty body)
        ;; This ensures defeasible derivations don't leak into strict reasoning
        (let [new-rule (defs/create-rule substituted-head (:type (:rule leaf)) [])]
          
          ;; Add rule to agenda if not already present
          (when-not (some #(= % new-rule) @(:agenda leaf))
            (swap! (:agenda leaf) conj new-rule))
          
          ;; Notify root with the derived literal
          (root-notify (:root leaf) substituted-head))))))

;; ============================================================================
;; Main RETE Algorithm - corresponds to Python fire_rules function (lines 100-132)
;; ============================================================================

(defn fire-rules
  "Fire all applicable rules using RETE algorithm.
   Corresponds to Python fire_rules function (lines 100-156)."
  [program]
  (let [agenda (atom (vec (defs/program-get-facts program)))
        facts (atom #{})
        root (create-root)]
    
    ;; Build RETE network for all non-fact rules
    (doseq [rule (remove defs/rule-fact? (:rules program))]
      (when (seq (:body rule))
        (let [body-patterns (:body rule)]
          (if (= 1 (count body-patterns))
            ;; Single pattern - create alpha node
            (let [alfa-node (create-alfa (first body-patterns) root)]
              (create-leaf rule alfa-node root agenda))
            
            ;; Multiple patterns - create beta nodes
            (loop [patterns body-patterns
                   current-node nil]
              (if (empty? patterns)
                (create-leaf rule current-node root agenda)
                (let [pattern (first patterns)]
                  (if (nil? current-node)
                    ;; First pattern - create alpha node
                    (let [alfa-node (create-alfa pattern root)]
                      (recur (rest patterns) alfa-node))
                    ;; Additional patterns - create beta nodes
                    (let [alfa-node (create-alfa pattern root)
                          beta-node (create-beta current-node alfa-node)]
                      (recur (rest patterns) beta-node))))))))))
    
    ;; Fire rules by processing agenda
    (while (seq @agenda)
      (let [current-fact (first @agenda)]
        (swap! agenda rest)
        (when-not (contains? @facts current-fact)
          (swap! facts conj current-fact)
          (root-notify root (:head current-fact)))))
    
    ;; Return all derived facts
    @facts))

;; ============================================================================
;; Debugging and Inspection Functions
;; ============================================================================

(defn describe-rete-network
  "Generate a description of the RETE network structure for debugging."
  [root]
  (letfn [(describe-node [node depth]
            (let [indent (apply str (repeat (* 2 depth) " "))
                  node-type (cond
                              (instance? ReteRoot node) "ROOT"
                              (instance? ReteAlfa node) (str "ALFA: " (:name node))
                              (instance? ReteBeta node) (str "BETA: " (:name node))
                              (instance? ReteLeaf node) (str "LEAF: " (:name node))
                              :else "UNKNOWN")]
              (str indent node-type "\n"
                   (when-let [children (and (not (instance? ReteLeaf node)) 
                                           @(:children node))]
                     (apply str (map #(describe-node % (inc depth)) children))))))]
    (describe-node root 0)))

(defn rete-network-stats
  "Get statistics about the RETE network structure."
  [root]
  (let [stats (atom {:root 1 :alfa 0 :beta 0 :leaf 0})
        visited (atom #{})]
    
    (letfn [(count-node [node]
              (when-not (contains? @visited node)
                (swap! visited conj node)
                (cond
                  (instance? ReteAlfa node) 
                  (do (swap! stats update :alfa inc)
                      (doseq [child @(:children node)]
                        (count-node child)))
                  
                  (instance? ReteBeta node)
                  (do (swap! stats update :beta inc)
                      (doseq [child @(:children node)]
                        (count-node child)))
                  
                  (instance? ReteLeaf node)
                  (swap! stats update :leaf inc))))]
      
      (doseq [child @(:children root)]
        (count-node child)))
    
    @stats))

;; ============================================================================
;; Export Public API
;; ============================================================================

(def public-api
  "Public API for RETE algorithm functionality."
  {:fire-rules fire-rules
   :create-root create-root
   :create-alfa create-alfa  
   :create-beta create-beta
   :create-leaf create-leaf
   :describe-rete-network describe-rete-network
   :rete-network-stats rete-network-stats})

;; ============================================================================
;; Integration with definitions.cljs
;; This resolves the forward reference in program-get-ground-program
;; ============================================================================

;; Note: This creates a circular dependency that needs to be handled carefully.
;; The definitions namespace references this namespace's fire-rules function.
;; In ClojureScript, we handle this by having definitions.cljs require this
;; namespace and call fire-rules directly rather than using a forward reference.
