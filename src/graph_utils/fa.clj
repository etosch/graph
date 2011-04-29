;; note: input is in lexicographic order, not order of being processed
;; i.e. it's reversed
(ns graph-utils.fa
  ^{:doc "Finite Automaton implementation"
    :author "Emma Tosch"}
  (:require [graph :only 'add-edge])
  (:use [graph :exclude 'add-edge]))

(defn add-edge
  [loc from to test transition-update read]
  (let [g (graph/add-edge loc from to test transition-update)]
    (if (= (edges g) (edges loc))
      g
      (with-meta g
	(assoc (meta g)
	  :edges (let [edge (find-first #(and (= from (:from %))
					      (= to (:to %)))
					(edges g))]
		   (cons (assoc edge :read read)
			 (remove #(= edge %) (edges g)))))))))
	  
(defn edge-transition-rule
  "General form of a fa transition rule."
  [from to read]
  (fn [loc input]
    (assert (meta loc))
    (make-graph loc to (conj (path loc) from)
		(cond (= read :epsilon)(:input (loc 1))
		      input [(rest (input-remaining loc))
			     (rcons read (input-processed loc))]
		      :else [nil (rcons read (input-processed loc))])
		nil)))

(defn edge-test
  [read]
  (fn [loc input] (assert (meta loc))
    (or (= read :epsilon) (not input) (= read input))))

(defn fa-graph
  "Returns a graph representing a finite automaton.

transition-map is a nested map where the keys are references to states and the values are maps from an emission to another state. Multiple destination states may be specified by referring to a sequence of states. Epsilon transitions are encoded with :epsilon. Other transitions are encoded with :other

start-node-map is a map specifying legal moves from start state 'start.

accept-node-set is a set of state references

input is a sequence or string to be processed. If the input is anything other than a sequence or string, graph is passed the empty list."
  [transition-map start-node-map accept-node-set input]
  (let [transition-map (merge transition-map start-node-map)]
    ;; I put earmuffs on the local functions to make them visually stand out.
    (letfn [(*accept-rule*
	     ([loc]
		(contains? accept-node-set (node loc)))
	     ([loc input]
		(and ((complement not-nil-or-empty?) input)
		     (*accept-rule* loc))))
	    (*reject-rule*
	     ([loc]
		(next-processor loc input))
	     ([loc input]
		(if ((complement not-nil-or-empty?) input)
		  (not (*accept-rule* loc))
		  ((complement not-nil-or-empty?) (next-processor loc input)))))]
      (graph (set (flatten (for [[from trans] transition-map] [from (vals trans)])))
	     (for [[from trans] transition-map
		   [read states] trans
		   to (if (coll? states) states (list states))]
	       {:from from :to to :test (edge-test read)
		:transition-update (edge-transition-rule from to read)
		:read read})
	     (cond (coll? input) input (string? input) (map str input) :else '())
	     *accept-rule*
	     *reject-rule*
	     nil))))

(defn enumerate [loc]
  ;; left-recursion issue with simpler implementations
  ;; memoization did NOT make this run faster
  (letfn [(step [steps loc]
		(assert (meta loc))
		(if (zero? steps)
		  (if ((accept-fn loc) loc) (list loc) '())
		  (mapcat #(step (dec steps) %) (next-generator loc))))
	  (step-wise [s] (concat (step s loc) (lazy-seq (step-wise (inc s)))))]
    (step-wise 1)))

;; (defn evaluate [loc]
;;   "Evaluates a single graph, given that it contains its input."
;;   (assert (meta loc))
;;   (letfn [(branches [loc]
;; 		    (assert (meta loc))
;; 		    (cond ((accept-fn loc) loc (first (input-remaining loc))) :accept
;; 			  ((reject-fn loc) loc (first (input-remaining loc))) :reject
;; 			  :else (map branches (next-processor loc))))]
;;     (or (find-first #(= :accept %) (flatten (branches loc))) :reject)))

 (defn evaluate [loc]
   (loop [remaining-branches (next-processor loc)]
     (let [this-loc (first remaining-branches)]
       (cond (nil? this-loc) :reject
	     ((accept-fn this-loc) this-loc (first (input-remaining this-loc))) :accept
	     :else (recur (concat (next-processor this-loc) (rest remaining-branches)))
	     ))))
	     

#_(def a*ab (fa-graph {'a* {"a" ['a* 'a*a]}
		      'a*a {"b" 'a*ab}}
		      {'start {"a" ['a* 'a*a] "b" 'dead :epsilon 'dead-2}}
		      #{'a*ab}
		      "aab"))

;;(map #(apply str %) (map input-processed (take 5 (enumerate a*ab))))


