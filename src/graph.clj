;; 22 March 2011 to do:
;; write macro for converting input to meta map
;; add changed? boolean
;; add a function to connect two given nodes
;; keep these implemented as closures - goal is to wrap push functions in transition functions
;; 23 March 2011
;; Reworked meta map in graph
;; Got rid of transition function - function is implicit in rules
;; Can't force form through explcit currying of function, since application is lazy anyway
;; - consider making this a multimethod?
;; wrote functions to make fas, pdas, lba
;; - consider writing a generic machine function
;; 28 March 2011
;; completely restructured graph
;; 29 March 2011
;; should consider complexity of push, pop, etc.

(ns ^{:doc "An immutable representation of a graph"
      :author "Emma Tosch"}
  graph
  (:gen-class)
  (:refer-clojure :exclude (replace remove next pop peek)))

(defstruct graph-struct :nodes :edges :transition-rules)

(defn make-graph
  "
 Creates a generic graph. Should be used with functions that are specific
 to different types of graphs. This function is implemented to provide fine-
 grained tuning of graphs during GP runs.

state-identifiers
   The set of identifiers corresponding to states in the graph.

edges
   The map of legal state connections, indexed on the state-indentifiers.

transition-rules
   The map of state transition rules, indexed on the state identifiers.
   Should be a curried function of two arguments; first is the current state
    of the graph, second is the input.

starts
   A vector of legal start states.

ads-identifier
   A keyword denoting the auxilary data structure identifier. Contained in
   the metadata, since it is determined per type.

aux-map
   A map of data specific to the instantiation. Typically contains the path
   and ads state.

terminal-rule
   A function that (1) checks to see if the computation should stop and
   (2) returns appropriate values. Should return nil if the computation is
   not complete. Contained in the metadata, since it is determined per type.
"
  [state-identifiers edges transition-rules starts ads-identifier aux-map termination-rule]
  (with-meta
    [starts aux-map (struct graph-struct state-identifiers edges transition-rules)]
    {:termination-rule termination-rule :ads-identifier ads-identifier}))

(defn node
  "Returns the current node in the graph"
  [loc]
  (loc 0))

(defn graph
  "Returns the current instantiation of the graph-struct"
  [loc]
  (loc 2))

(defn path
  "Returns a list of the nodes already visited. Used in prev."
  [loc]
  (:pnodes (loc 1)))

(defn ads
  "Returns the current state of the auxilary data structure."
  [loc]
  ((or (loc 1) {}) (:ads-identifier (meta loc))))

(defn alpha
  "Returns the set corresponding to the alphabet, if applicable.
Otherwise, nil."
  [loc]
  (:alpha (loc 1)))

(defn transition-function
  "Returns the transition function for the current graph"
  [loc]
  (:transition-rules (graph loc)))

(defn push
  "Pushes the item onto a stack and returns a new stack."
  [item stack]
  (conj stack item))

(defn pop
  "Removes the top item from a stack and returns a new stack without it."
  [stack]
  (rest stack))

(defn peek
  "Returns the top item on the stack without removing it."
  [stack]
  (first stack))

(defn ensure-list-no-nil
  "For use in generalizing nondeterministic exploration of graphs. If the
returned value of a transition function is a list, return the item. If the
returned value of a transition function is not a list, wrap it in one.
Removes nils from the returned list."
  [returned-item]
  (clojure.core/remove nil? (if (list? returned-item)
			      returned-item
			      (list returned-item))))

(defn update-graph-slot
  "Updates a loc's graph-struct at the supplied key with the supplied
function. Returns a new value for this slot."
  [loc key fun]
  (fun (key (graph loc))))

(defn update-ads
  "Updates the auxilary data structure of the supplied loc using the function
supplied. Returns a new loc."
  [loc fun]
  (with-meta
    [(node loc)
     (assoc (loc 1)(:ads-identifier (meta loc)) (fun (ads loc)))
     (loc 2)]
    (meta loc)))

(defn update-node
  "Updates the node of the supplied loc with the supplied n. Returns a new
loc."
  [loc n]
  (with-meta
    [n
     (loc 1)
     (loc 2)]
    (meta loc)))

(defn fa-graph
  "
Creates a graph representing a finite automaton.

state-transition-table
   A map whose keys are state identifiers and whose values are curried
   functions of two variables. The first corresponds to a loc, the second to
   some input. Is implemented to be nondeterministic; transition function
   returns a vector of applicable transitions.

set-of-accept-nodes

"
  [state-transition-table set-of-accept-nodes set-of-start-nodes dead-state-id]
  (make-graph (keys state-transition-table) 
	      (zipmap (keys state-transition-table) (map vals (vals state-transition-table)))
	      (fn [loc]
		(fn [input]
		  (vec (for [states (ensure-list-no-nil ((state-transition-table (node loc)) input))]
			 (update-node loc states)))))
	      set-of-start-nodes
	      nil
	      {:pnodes nil :alpha (set (flatten (map keys (vals state-transition-table))))}
	      (fn [loc]
		(fn [input]
		  (cond (= (node loc) dead-state-id) :reject
			(and (nil? input) (contains? set-of-accept-nodes (node loc))) :accept
			(and (nil? input) (not (contains? set-of-accept-nodes) (node loc))) :reject)))))

(defn pda-graph
  [state-transition-table set-of-accept-nodes set-of-start-nodes dead-state-id transition-rule-map]
  (make-graph (keys state-transition-table)
	 state-transition-table
	 (fn [loc]
	   (fn [input]
	     (vec (for [[n fun] (ensure-list-no-nil (((transition-rule-map (node loc)) input) loc))]
		    (update-ads (update-node loc n) fun)))))
	 set-of-start-nodes
	 :stack
	 {:pnodes nil :stack nil}
	 (fn [loc]
	   (fn [input]
	     (cond (= (node loc) dead-state-id) :reject
		   (and (nil? input) (empty? (ads loc)) (contains? set-of-accept-nodes (node loc))) :accept
		   (and (nil? input) (empty? (ads loc)) (not (contains? set-of-accept-nodes (node loc)))) :reject
		   (and (nil? input) (not (empty? (ads loc)))) :reject)))))

;; (defn ann-graph
;;   [input-states output-states hidden-states]
;;   (make-graph (

(defn add-state
  [loc n connections transition-function]
  (with-meta [(node loc)
	      (loc 1)
	      (struct graph-struct
		      (update-graph-slot loc :nodes #(conj % n))
		      (update-graph-slot loc :edges #(conj % connections))
		      (update-graph-slot loc :transition-rules #(conj % transition-function)))]
    (meta loc)))
		      
    
(defn remove-state
  [loc state]
  (with-meta [(node loc)
	      (loc 1)
	      (struct graph-struct
		      (update-graph-slot loc :nodes #(disj % state))
		      (update-graph-slot loc :edges #(dissoc % state))
		      (update-graph-slot loc :transition-rules #(dissoc % state)))]
    (meta loc)))
       
(defn next [loc i]
  (let [continue (((:termination-rule (meta loc)) loc) i)]
    (if (not continue)
      continue
      (with-meta (transition loc) (meta loc)))))
     
;;     ((:terminal-action (meta loc)) loc)
;;     (with-meta 
;;       [((:transition-function (meta loc)) loc i)
;;        (assoc (loc 1) :pnodes (conj (path loc) (node loc)))]
;;       (meta loc))))

;; (defn prev [loc]
;;   (if (start? loc)
;;     loc
;;     (with-meta
;;       [(first (path loc))
;;        (when (not (empty? (rest (path loc))))
;; 	 (assoc (loc 1) :pnodes (rest (path loc))))]
;;       (meta loc))))


(defn max-num-edges
  "Returns the number of edges in a physical model (nothing is counted twice)"
  [num-nodes directed?]
  (if directed?
    (+ num-nodes (reduce + (range num-nodes)))
    (+ num-nodes (* (dec num-nodes) num-nodes))))

(defn-
  ^{:test (fn [] (assert (fully-connected? {:a #{:a}} true)))}
  fully-connected?
  [transition-map directed?]
    (= (max-num-edges (count (keys transition-map)) directed?)
       (count (flatten (vals transition-map)))))

(defn-
  ^{:test (fn [] (assert (and (not (choose-random-unconnected-nodes {:a #{:a}} true))
			      (let [[from to] (choose-random-unconnected-nodes {:a #{:a :b} :b #{:a}} false)]
				(and (= from :b) (= to :b)))
			      (let [[from to] (choose-random-unconnected-nodes {:a #{:a :b} :b #{:b}} true)]
				(and (= from :b) (= to :a)))
			      )))}
  choose-random-unconnected-nodes
  [transition-map directed?]
  (loop [nodes (keys transition-map)
	 from (rand-nth nodes)
	 to (rand-nth nodes)]
    (if (= (count (transition-map from)) (count nodes))
      (and (not (fully-connected? transition-map directed?))
	   (recur nodes (rand-nth nodes) to))
      (if (contains? (transition-map from) to)
	(do (if (not directed?)
	      (assert (contains? (transition-map to) from)))
	    (recur nodes (rand-nth nodes) (rand-nth nodes))) 
	[from to]))))
	
(defn generate-random-graph
  ([num-nodes density directed?]
     (loop [connections (int (* (max-num-edges num-nodes directed?) density))
	    transitions (apply hash-map (interleave (for [_ (range num-nodes)] (keyword (gensym)))
						    (repeat #{})))]
       (if (zero? connections)
	 (graph (keys transitions)
		transitions
		(fn [selection-function input] (selection-function transitions))
		keys
		nil
		(fn [loc] (zero? (count (transitions loc))))
		identity)
	 (let [[from to] (choose-random-unconnected-nodes transitions directed?)]
	   (if (or directed? (= from to))
	     (recur (dec connections)
		    (assoc transitions from (conj (transitions from) to)))
	     (recur (dec (dec connections))
		    (-> (assoc transitions from (conj (transitions from) to))
			((eval (fn [tmap] (assoc tmap to (conj (tmap to) from)))))))
	     )))))
  ([num-nodes density] (generate-random-graph num-nodes density false)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-deterministic-regexp
     ^{:doc "1+0+"}
     (fa-graph {"1" {"1" "1+", "0" "dead"},
		"1+" {"1" "1+", "0" "0"},
		"0" {"0" "0+", "1" "dead"},
		"0+" {"0" "0+", "1" "dead"}}
	       {"0+"}
	       ["1"]
	       "dead"))

(def test-nondeterministic-regexp
     ^{:doc "(a*ab)"}
     (fa-graph {'a1 {"a" ['a1 'a2]}
		'a2 {"b" 'b}}
	       #{'b}
	       #{'a1 'a2}
	       nil))

(def test-deterministic-pda
     ^{:doc "Match parentheses."}
     (pda-graph {'accept ['unbalanced 'dead]
		 'unbalanced ['unbalanced 'accept 'dead]
		 'dead ['dead]}
		#{'accept}
		#{'accept}
		'dead
		{'accept {"(" (fn [loc] ['unbalanced (fn [stack] (push 'lparen stack))])
			  ")" (fn [loc] ['dead identity])}
		 'unbalanced {"(" (fn [loc] ['unbalanced (fn [stack] (push 'lparen stack))])
			      ")" (fn [loc] (if (empty? (ads loc)) ['dead identity] ['unbalanced pop]))}
		 'dead {}}))

(def test-nondeterministic-pda
     ^{:doc "palindromes"}
     (pda-graph {'accept ['spin 'dead]}
		#{'backward}
		#{'start}
		'dead
		{'start (fn [input]
			   (fn [loc]
			     ['forward (fn [stack] (push input stack))]))
		 'forward (fn [input]
			    (fn [loc]
			      (list ['forward (fn [stack] (push input stack))]
				    ['backward identity]
				    (if (= input (peek (ads loc))) ['backward pop]))))
		 'backward (fn [input]
			     (fn [loc]
			       (if (= input (peek (ads loc))) ['backward pop] ['dead identity])))
		 }))
		 
(defn test-functions [g]
  (do (print "node: " (node g) "\n"
	     "path: " (path g) "\n"
	     "ads: " (ads g) "\n")
      'success))

(for [g '(test-deterministic-regexp
	  test-nondeterministic-regexp
	  test-deterministic-pda
	  test-nondeterministic-pda)]
  (test-functions g))