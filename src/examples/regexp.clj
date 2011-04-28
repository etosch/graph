;; regexp.clj
(ns examples.regexp
  (:use [clojush] [graph :exclude [rcons]]
	[graph-utils.fa :exclude [rcons]]))                                  

(def a*ab
     (enumerate (fa-graph {'a* {"a" ['a* 'a*a]}
			   'a*a {"b" 'accept}}
			  {'start {"a" ['a* 'a*a] "b" 'dead}}
			  #{'accept}
			  nil)))
(def alphabet '("a" "b"))
(def first-100-regexps (take 100 a*ab))
(def regexp-instructions '(start add-unconnected-node add-connected-node
				 remove-node add-edge remove-edge next
			   	 prev generate-empty-graph))

(defn is-graph? [loc]
  (and (vector? loc)
       (symbol? (first loc))
       (map? (loc 1))
       (meta loc)))

(defn eval-machine [string loc]
  (if (is-graph? loc)
    (evaluate (with-input (move-graph loc 'start) string))
    (do (println "loc is not a graph: " loc)
	:reject)))


(defn new-empty-graph []
  (fa-graph {'start {:epsilon 'accept}}
	    {}
	    #{'accept}
	    nil))

#_(-> (new-empty-graph)
    (add-node 'A)
    (add-node 'B)
    (add-edge 'start 'A (edge-test "a") (edge-transition-rule 'start 'A "a"))
    (add-edge 'start 'B (edge-test "a") (edge-transition-rule 'start 'B "a"))
    (add-edge 'A 'A (edge-test "a") (edge-transition-rule 'A 'A "a"))
    (add-edge 'A 'B (edge-test "a") (edge-transition-rule 'A 'B "a"))
    (add-edge 'B 'accept (edge-test "b") (edge-transition-rule 'B 'accept "b"))
    (with-input "b")
    (evaluate))
    

(defn regexp-fitness
  [program]
  (doall
   (->> (for [regexp (map input-processed (take 10 (shuffle first-100-regexps)))]
	  (->> (push-item (new-empty-graph) :auxiliary (make-push-state))
	       ;;	       (#(run-push program % true))
	       (run-push program)
	       (:auxiliary)
	       (first)
	       (eval-machine regexp)))
	(filter #(= :accept %))
	(count)
	(- 10)
	(list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push graph instructions

(define-registered generate-empty-graph
  (fn [state]
    (push-item (new-empty-graph) :auxiliary state)))

;; moves graph to the start node
(define-registered start
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))
      (let [loc (top-item :auxiliary state)
	    g (move-graph loc 'start)]
	(if (is-graph? g)
	  (push-item g :auxiliary (pop-item :auxiliary state))
	  state))
      state)))
      
	  

;; add completely new node
(define-registered add-unconnected-node
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))
      (let [loc (top-item :auxiliary state)
	    g (add-node loc (gensym))]
	(if (is-graph? g)
	  (push-item g :auxiliary (pop-item :auxiliary state))
	  state))
      state)))

(define-registered add-connected-node
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))
      (let [loc (top-item :auxiliary state)
	    new-node-name (gensym)
	    g (add-node loc new-node-name)]
	(if (is-graph? g)
	  (-> (add-edge g (node g) new-node-name
			#(or true %2)
			(fn [loc input]
			  (make-graph loc (node loc)
				      (conj (path loc) (node loc))
				      (:input (loc 1)) nil)))
	      (push-item :auxiliary (pop-item :auxiliary state)))
	  state))
      state)))

;; remove the node that the graph is currently at
(define-registered remove-node
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))
      (let [loc (top-item :auxiliary state)
	    g (remove-node loc (node loc))]
	(if (and (not= (node loc) 'start)
		 (not= (node loc) 'accept)
		 (is-graph? g))
	  (push-item g :auxiliary (pop-item :auxiliary state))
	  state))
      state)))
	  
;; remove a random node in the graph
(define-registered remove-random-node
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))
      (let [loc (top-item :auxiliary state)
	    g (remove-node loc (rand-nth (seq (nodes loc))))]
	(if (and (not= (node loc) 'start)
		 (not= (node loc) 'accept)
		 (is-graph? g))
	  (push-item g :auxiliary (pop-item :auxiliary state))
	  state))
      state)))

;; add an edge between the last location and two before where the read value
;; is the last input read
(define-registered add-edge
  (fn [state]
    (if (and (n-on-stack? 3 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state))
	     (not= :no-stack-item (stack-ref :auxiliary 2 state)))
      (let [loc (stack-ref :auxiliary 0 state)
	    loc-2 (stack-ref :auxiliary 2 state)
	    read (or (first (path loc)) (rand-nth alphabet))]
	(if (and (contains? (nodes loc) (node loc-2))
		 (seq (next-processor loc read)))
	  (push-item (add-edge loc (node loc) (node loc-2)
			       (edge-test read)
			       (edge-transition-rule (node loc) (node loc-2) read))
		     :auxiliary
		     (pop-item :auxiliary state))
	  state))
      state)))
    

;; add a random edge between two nodes in the graph.
(define-registered add-random-edge
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))	     
      (let [loc (top-item :auxilary state)
	    read (rand-nth alphabet)
	    from (rand-nth (seq (nodes loc)))
	    to (rand-nth (seq (nodes loc)))]
	(if (and (is-graph? loc)
		 (contains? (map node (if (seq? (next-processor loc read))
					(next-processor loc read)
					(list (next-processor loc read))))
			    to))
	  state
	  (push-item (add-edge loc from to
			       (edge-test read)
			       (edge-transition-rule from to read))
		     :auxiliary
		     (pop-item :auxiliary state))))
      state)))

;; remove the last transition
(define-registered remove-edge
  (fn [state]
    (if (and (n-on-stack? 3 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state))
	     (not= :no-stack-item (stack-ref :auxiliary 2 state)))
      (let [loc (stack-ref :auxiliary 0 state)]
	(if (or (= (node loc) 'start)
		(= (node loc) 'accept))
	  state
	  (let [loc-2 (stack-ref :auxiliary 2 state)
		g (remove-edge loc (node loc) (node loc-2))]
	    (if (is-graph? g)
	      (push-item g :auxiliary (pop-item :auxiliary state))
	      state))))
	state)))

;; remove a random edge
(define-registered remove-random-edge
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))
      (let [loc (top-item :auxiliary state)]
	(if (or (= (node loc) 'start)
		(= (node loc) 'accept))
	  state
	  (let [edge (rand-nth (seq (edges loc)))
		g (remove-edge loc (:from edge) (:to edge))]
	    (if (is-graph? g)
	      (push-item g :auxiliary (pop-item :auxiliary state))
	      state))))
      state)))

;; pushes the set of next available nodes onto the stack
(define-registered next
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
	     (not= :no-stack-item (top-item :auxiliary state)))	     	     
      (let [top (top-item :auxiliary state)
	    loc (if (seq? top) top (list top))
	    nd-locs (mapcat next-generator loc)]
	(if (empty? nd-locs)
	  state
	  (loop [nd-locs nd-locs s (pop-item :auxiliary state)]
	    (if (empty? nd-locs)
	      s
	      (recur (rest nd-locs)
		     (push-item (first nd-locs) :auxiliary s))))))
      state)))

;; move the graph back to the last valid previous location
(define-registered prev
  (fn [state]
    (if (and (n-on-stack? 1 :auxiliary state)
      	     (not= :no-stack-item (top-item :auxiliary state)))
      (let [loc (top-item :auxiliary state)]
	(push-item (prev loc) :auxiliary (pop-item :auxiliary state)))
      state)))

;;aux function used in the fitness function  



#_(pushgp :error-function regexp-fitness
	  :atom-generators regexp-instructions
	  :max-generations 5
	  :population-size 10
	  :max-points 10)