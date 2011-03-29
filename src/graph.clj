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

(ns ^{:doc "An immutable representation of a graph, closely modelled on
on Rich Hickey's zipper implementation (clojure.zip)."
      :author "Emma Tosch"}
  graph
  (:gen-class)
  (:refer-clojure :exclude (replace remove next pop peek)))

(defstruct graph-struct :nodes :edges :transition-rules)

(defn graph
  [state-identifiers edges transition-rules starts ads-identifier aux-map termination-rule]
  (with-meta
    [starts aux-map (struct graph-struct state-identifiers edges transition-rules)]
    {:termination-rule termination-rule :ads-identifier ads-identifier}))

(defn node [loc] (loc 0))

(defn path [loc] (:pnodes (loc 1)))

(defn ads [loc] ((or (loc 1) {}) (:ads-identifier (meta loc))))

(defn alpha [loc] (:alpha (loc 1)))

(defn push [item stack] (cons item stack))

(defn pop [stack] (rest stack))

(defn peek [stack] (first stack))

(defn update-ads
  [loc fun]
  (with-meta
    [(node loc) (assoc (loc 1) (:ads-identifier (meta loc)) (fun (ads loc))) (loc 2)]
    (meta loc)))

(defn update-node
  [loc n]
  (with-meta [n (loc 1) (loc 2)]
    (meta loc)))

(defn ensure-list-no-nil [l] (clojure.core/remove nil? (if (list? l) l (list l))))

(defn fa-graph
  [state-transition-table set-of-accept-nodes set-of-start-nodes dead-state-id]
  (graph (keys state-transition-table) 
	 (zipmap (keys state-transition-table) (map vals (vals state-transition-table)))
	 (fn [loc]
	   (fn [input]
	     (vec (for [states (ensure-list-no-nil ((state-transition-table (node loc)) input))]
		    (update-node loc states)))))
	 set-of-start-nodes
	 nil
	 {:pnodes nil :alpha (set (map keys (vals state-transition-table)))}
	 (fn [loc]
	   (fn [input]
	     (cond (= (node loc) dead-state-id) :reject
	 	   (and (nil? input) (contains? set-of-accept-nodes (node loc))) :accept
	 	   (and (nil? input) (not (contains? set-of-accept-nodes) (node loc))) :reject)))))

(defn pda-graph
  [state-transition-table set-of-accept-nodes set-of-start-nodes dead-state-id transition-rule-map]
  (graph (keys state-transition-table)
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

(defn make-node [loc state-identifier transition-rules]
  "Makes a new node of the same type as loc."
  (with-meta [state-identifier {:transition-rules transition-rules}]
    (meta loc)))

;; (defn next [loc i]
;;   (if (nil? i)
;;     (
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
	       #{"0+"}
	       #{"1"}
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