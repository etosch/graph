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

(ns ^{:doc "An immutable representation of a graph, closely modelled on
on Rich Hickey's zipper implementation (clojure.zip)."
      :author "Emma Tosch"}
  graph
  (:gen-class)
  ;; (:require [clojure.contrib.math :as math]
  ;; 	    [clojure.contrib.graph :as graph])
  (:refer-clojure :exclude (replace remove next)))

(defn graph
  "states is a collection of unique state identifiers. Since transition-rules is usually implemented
as a map, this can be filched from its keys.

ads is the auxilary data structure. 

transition-rules is a curried function whose argument is a graph in some current location.
It returns a function. A typical implementation is a function that takes in some input from some
exterior process or data structure and returns a new state, given that input.

terminal-action is a function that takes in a graph and returns some appropriate terminal value.

Returns an empty vector.
"
  [states ads-update-rule transition-rules constructor terminal-action]
  (with-meta
    (constructor {:transition-rules transition-rules :states states :pnodes nil})
    {:ads ads-update-rule
     :constructor constructor
     :terminal-action terminal-action
     }))


(defn node
  [loc]
  (loc 0))

(defn fa-graph
 ;; for now, this just runs through the entire input; doesn't terminate when it hits a dead state
  [state-transition-table set-of-accept-nodes start-rule dead-state-id]
  (graph (keys state-transition-table) 
	 identity
	 (fn [loc] (if (nil? (node loc))
		     start-rule
		     (state-transition-table (node loc) (fn [_] dead-state-id))))
	 (fn [m] [nil m])
	 (fn [loc] (if (contains? set-of-accept-nodes (node loc)) :accept :reject))))

(defn pda-graph
 ;; for now, this just runs through the entire input; doesn't terminate when it hits a dead state
  [state-transition-table ads-update-rule set-of-accept-nodes start-rule accept-ads-condition dead-state-id]
  (graph (keys state-transition-table) ;;states
	 ads-update-rule
	 (fn [loc] (cond (nil? (node loc)) start-rule
			 (nil? (state-transition-table (node loc))) (fn [_] dead-state-id)
			 :else ((state-transition-table (node loc)) (:stack (loc 1)))))
	 (fn [m] [nil (assoc m :stack (list))])
	 (fn [loc] (if (and (contains? set-of-accept-nodes (node loc))
			    (accept-ads-condition loc))
		     :accept :reject))))

(defn lba-graph
 ;; for now, this just runs through the entire input; doesn't terminate when it hits a dead state
  [state-transition-table set-of-accept-nodes start-rule dead-state-id]
  (graph (keys state-transition-table) ;;states
	 (vector)
	 (fn [loc] (if (nil? (node loc))
		     start-rule
		     (state-transition-table (node loc) (fn [_] dead-state-id))))
	 (fn [m] [nil m])
	 (fn [loc] (if (contains? set-of-accept-nodes (node loc)) :accept :reject))))

(defn hmm-graph
  "The HMM can be specialized for decoding, etc."
  [observation-parameters hidden-state-parameters starting-probabilites transition-operation terminal-action]
  (graph (keys hidden-state-parameters)
	 (interleave (keys hidden-state-parameters) {})
	 transition-operation
	 (fn [m] m)
	 terminal-action))

(defn make-node [loc state-identifier transition-rules]
  "Makes a new node of the same type as loc."
  (with-meta [state-identifier {:transition-rules transition-rules}]
    (meta loc)))

(defn- ensure-set [s]
  (cond (set? s) s
	(seq? s) (set s)
	:else (set (list s))))

(defn path
  "Modelled directly on the zip path implementation"
  [loc]
  (:pnodes (loc 1)))

(defn next [loc i]
  (if (nil? i)
    ((:terminal-action (meta loc)) loc)
    (with-meta 
      [(((:transition-rules (meta loc)) loc) i)
       (assoc (loc 1) :pnodes (conj (path loc) (node loc)))]
      (meta loc))))

(defn start?
  [loc]
  (empty? (path loc)))


(defn prev [loc]
  (if (start? loc)
    loc
    (with-meta
      [(first (path loc))
       (when (not (empty? (rest (path loc))))
	 (assoc (loc 1) :pnodes (rest (path loc))))]
      (meta loc))))
      
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



(comment
  
(def undirected-graph (generate-random-graph 10 0.5))
(def directed-graph (generate-random-graph 10 0.5 true))
;; get back the transition map:
;;(((meta undirected-graph) :graph/transition) nil (fn [a b] b))
;;(((meta directed-graph) :graph/transition) nil (fn [a b] b))

(def
 ^{:doc "11*00*"}
 test-regexp
 (graph
  #{"start" "1" "1+" "1+0" "1+0+" "dead"}
  {"start" {"1" "1"}
   "1" {"1" "1+" "0" "1+0"}
   "1+" {"1" "1+" "0" "1+0"}
   "1+0" {"0" "1+0+"}
   "1+0+" {"0" "1+0+"}
   "dead" {}}
  (fn [loc] (fn [input] (((:neighbors (meta loc)) (node loc)) input "dead")))
  (fn [_] "start")
  nil
  (fn [loc] (or (= (node loc) "dead")
		(= (node loc) :reject)))
  (fn [g] (if (= (node g) "1+0+") :accept :reject))))
)

;; (defn get-neighbors
;;   "Based on get-neighbors in the graph namespace, this version
;; gets neighbors based upon the current location of the graph"
;;   [loc]
;;   (graph/get-neighbors (meta loc) (node loc)))
