;; currently only alters interior nodes
(ns graph
  ^{:doc "Functionally encoding graphs. Styled after zippers and cellular encoding."
    :author "Emma Tosch"}
  (:gen-class)
  (:require [clojure.set :as set] [aux]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graph functions

(defn graph
  "Returns a generalized graph data structure. Should be specialized for graph types. See fa-graph, pda-graph, ann-graph, etc. All associated functions are written to handle non-determinism. All graphs being on start state 'start.

nodes is a set of distinct node references in the graph.

edges is a map of legal edges in the graph, along with a test value and a transition function, if needed.

input is the input being processed. Currently I assume these are strings or sequences.

accept-rule is a fn that returns true only when the graph is in an accepting state.

reject-rule is a fn that returns true only when the graph is in a rejecting state.

aux-struct is an auxilary data structure that may be updated as the graph processes input. It can take an initial value or a fn corresponding to the type of data structure it is.
"
  [nodes edges input accept-rule reject-rule aux-struct]
  (assert (set/subset? (set/union (set (map :from edges))
				  (set (map :to edges))) nodes))
  (with-meta 
    ['start {:path '()
	     :input [input '()]
	     :aux-struct (if (fn? aux-struct) (aux-struct) aux-struct)}]
    {:transition-rule (fn [loc]
			(assert (meta loc))
			 (fn [input]
			   (let [transitions (transient [])]
			     (doseq [edge (:edges (meta loc))]
			       (if (and (= (loc 0) (:from edge))
			   		((:test edge) loc input))
			   	(conj! transitions
			   	       ((:transition-update edge) loc input))))
			     (seq (persistent! transitions)))))
				  
			  ;; (keep #(if ((:test %) loc input)
			  ;; 	   ((:transition-update %) loc input))
			  ;; 	(filter #(= (loc 0) (:from %)) (:edges (meta loc)))))) ;; also tried with mapcar, filtering out nils from a for loop
     :nodes nodes :edges edges
     :accept-rule accept-rule :reject-rule reject-rule}))

(defn make-graph
  "Returns a new graph of the same type as input graph loc, with new instance data."
  [loc state path input aux-struct]
  (assert (meta loc))
  (with-meta [state {:path path :input input :aux-struct aux-struct}]
    (meta loc)))

;; accessors
(defn node [loc] (loc 0))
(defn path [loc] (:path (loc 1)))
(defn input-remaining [loc] ((:input (loc 1)) 0))
(defn input-processed [loc] ((:input (loc 1)) 1))
(defn with-input [loc input-string]
  (with-meta [(node loc)
	      (assoc (loc 1) :input [(map str input-string) '()])]
    (meta loc)))
(defn nodes [loc] (:nodes (meta loc)))
(defn edges [loc] (:edges (meta loc)))
(defn aux-struct [loc] (:aux-struct (loc 1)))
(defn accept-fn [loc] (:accept-rule (meta loc)))
(defn reject-fn [loc] (:reject-rule (meta loc)))
(defn transition-fn
  ([loc input] (((:transition-rule (meta loc)) loc) input))
  ([loc] (transition-fn loc nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; movement operations
;; next and prev functions operate on either a single
;; graph instance or a seq of instances

(defn next-generator
  [loc]
  (if (seq? loc)
    (loop [this-gen loc next-gen '()]
      (if (empty? this-gen) next-gen
	  (recur (rest this-gen) (concat (transition-fn (first this-gen) false) next-gen))))
    (transition-fn loc false)))

(defn next-processor
  ([loc input]
     (if (seq? loc)
       (loop [this-gen loc next-gen '()]
	 (if (empty? this-gen) next-gen
	     (recur (rest this-gen) (concat next-gen (transition-fn (first this-gen) input)))))
       (transition-fn loc input)))
  ([loc]
     (if (seq? loc)
       (loop [this-gen loc next-gen '()]
	 (if (empty? this-gen) next-gen
	     (recur (rest this-gen) (concat next-gen (transition-fn (first this-gen) (first (input-remaining (first this-gen))))))))
       (transition-fn loc (first (input-remaining loc))))))

(defn prev
  [loc]
  (letfn [(move-prev
	   [lloc] (if-let [prev-legal-node (aux/find-first #(contains? (nodes lloc) %) (path lloc))]
		    (with-meta [prev-legal-node (assoc (lloc 1)
						  :path (cons prev-legal-node (path lloc)))]
		      (meta lloc))
		    lloc))]
    (if (seq? loc)
      (map move-prev loc)
      (move-prev loc))))


(defn move-graph
  "Picks up and moves a graph without reading input.
If the supplied to node is not in the graph, returns loc."
  [loc n]
  (if (contains? (nodes loc) n)
    (with-meta [n (assoc (loc 1) :path (cons n (path loc)))]
      (meta loc))
    loc))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; topology altering functions

(defn add-node
  "Adds new interior nodes without edges."
  [loc n]
  (with-meta loc
    (assoc (meta loc)
      :nodes (conj (nodes loc) n))))
    
(defn remove-node
  "Removes all interior nodes and attached edges. "
  [loc n]
  (if-not (or (coll? n) (not n))
    (with-meta (if (= n (node loc)) (prev loc) loc)
      (assoc (meta loc)
	:nodes (disj (nodes loc) n)
	:edges (clojure.core/remove #(or (= (:from %) n) (= (:to %) n)) (edges loc))))
    loc))
	   
(defn add-directed-edge
  "Adds a new edge between two existing nodes.
Allows multiple edges between nodes, allowing for multiple transition functions."
  [loc from to test transition-update]
  (if (set/subset? (set (list from to)) (nodes loc))
    (with-meta loc
      (assoc (meta loc)
	:edges (conj (edges loc)
		     {:from from :to to
		      :test test :transition-update transition-update})))
    loc))

(defn add-undirected-edge
  [loc from to test transition-update]
  (add-directed-edge loc from to test transition-update)
  (add-directed-edge loc to from test transition-update))

(defn remove-edge
  "Removes an edge from the graph. If the edge indicated does not exist, just returns the graph. If there are multiple edges between nodes, this function will remove all of them. This takes care of the case of undirected graphs."
  [loc from to]
  (if (and from to (not (coll? from)) (not (coll? to)))
    (with-meta loc
      (assoc (meta loc)
	:edges (clojure.core/remove #(and (= (:from %) from) (= (:to %) to)) (edges loc))))
   loc))
