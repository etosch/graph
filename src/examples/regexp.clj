;; regexp.clj
(ns examples.regexp
  (:use [clojush] [graph :exclude [rcons add-edge]]
	[graph-utils.fa :exclude [rcons]]))                                  

(def *verbose* (atom false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vars to be used
(def aa*b
     (fa-graph {'a* {"a" ['a* 'a*a]}
		'a*a {"b" 'accept}}
	       {'start {"a" ['a* 'a*a] "b" 'dead}}
	       #{'accept}
	       nil))
(def alphabet '("a" "b"))
(def first-100-regexps (take 100 (enumerate aa*b)))
(def regexp-instructions (cons (fn [] (rand-int 50))
			       '(start
				 nth-next
				 prev
				 nth-move
				 add-unconnected-node
				 add-connected-node
				 remove-node
				 add-edge
				 remove-edge
				 add-connected-nodes
				 connect-to-accept
				 )))
     
(defn is-graph? [loc]
  (and (vector? loc)
       (symbol? (first loc))
       (map? (loc 1))
       (meta loc)
       loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modificationin clojush to print the best graph
(in-ns 'clojush)
(use 'graph)
(reset! global-error-reuse false)
(def best-graph (atom ()))
       
(defn graph-display [loc]
  (when (seq loc)
    (list loc
	  {:nodes (nodes loc)
	   :edges (map #(select-keys % '(:from :to :read)) (edges loc))
	   :accept-nodes (map node
			      (filter #((accept-fn loc) %)
				      (map #(move-graph loc %) (nodes loc))))
	   })))

(defn problem-specific-report
  [best population generation error-function report-simplifications]
  (println "\nBest graph:" (graph-display (:graph (find-first
						   #(and (= (:code %)
							    (:program best))
							 (= (ensure-list (:fitness %))
							    (:errors best)))
						   @best-graph)))))
(defn stack-safe? [type state & [n]]
  (and (n-on-stack? (or n 1) type state)
       (not-any? #(or (= :no-stack-item %)
		      (nil? %)
		      (and (coll? %) (empty? %)))
		 (take (or n 1) (type state)))))

(in-ns 'examples.regexp)
(use 'clojush)


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
    (add-edge 'start 'A (edge-test "a") (edge-transition-rule 'start 'A "a") "a")
    (add-edge 'start 'B (edge-test "a") (edge-transition-rule 'start 'B "a") "a")
    (add-edge 'A 'A (edge-test "a") (edge-transition-rule 'A 'A "a") "a")
    (add-edge 'A 'B (edge-test "a") (edge-transition-rule 'A 'B "a") "a")
    (add-edge 'B 'accept (edge-test "b") (edge-transition-rule 'B 'accept "b") "b")
    (with-input "aab")
    (evaluate))

;; need to test this
(defn regexp-fitness
  [program]
  (let [state (run-push program (push-item (new-empty-graph) :auxiliary (make-push-state)) @*verbose*)
	fit-1 (->> (for [regexp (map input-processed (take 5 (shuffle first-100-regexps)))]
		     (eval-machine regexp (top-item :auxiliary state)))
		   (filter #(= :accept %))
		   (count)
		   (- 5))
	;; fit-2 (->> (for [regexp (map input-processed (take 5 (enumerate (top-item :auxiliary state))))]
	;; 	     (eval-machine regexp aa*b))
	;; 	   (filter #(= :accept %))
	;; 	   (count)
	;; 	   (- 5))
	;;	fitness (+ fit-1 fit-2)]
	fitness fit-1]
    (cond (or (not (seq @best-graph))
	      (> (:fitness (first @best-graph)) fitness))
    	  (reset! best-graph
		  (list {:fitness fitness :code program
			 :graph (top-item :auxiliary state)}))
	  (= (:fitness (first @best-graph)) fitness)
	  (swap! best-graph conj {:fitness fitness :code program
				  :graph (top-item :auxiliary state)}))
    (list fitness)))


#_(-> (random-code 10 regexp-instructions)
      (regexp-fitness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push graph instructions

;; moves the graph to the start node
(define-registered start
  (fn [state]
    (if-let [g (and (n-on-stack? 1 :auxiliary state)
		    (not= :no-stack-item (top-item :auxiliary state))
		    (is-graph? (move-graph (top-item :auxiliary state) 'start)))]
      (push-item g :auxiliary (pop-item :auxiliary state))
      state)))

;; pushes the set of next available nodes onto the stack
(define-registered nth-next
  (fn [state]
    (if (and (stack-safe? :auxiliary state)
	     (stack-safe? :integer state)
	     (is-graph? (top-item :auxiliary state)))
      (let [next-locs (next-generator (top-item :auxiliary state))]
	(if-let [g (and (not (empty? next-locs))
			(is-graph? (nth next-locs
					(mod (top-item :integer state)
					     (count next-locs)))))]
	    (push-item g :auxiliary (pop-item :auxiliary state))
	    state)))
      state))

;; move the graph back to the last valid previous location
(define-registered prev
  (fn [state]
    (if-let [g (and (stack-safe? :auxiliary state)
		    (is-graph? (top-item :auxiliary state))
		    (is-graph? (prev (top-item :auxiliary state))))]
      (push-item g :auxiliary (pop-item :integer state))
      state)))

(define-registered nth-move
  (fn [state]
    (if-let [g (and (stack-safe? :auxiliary state)
		    (stack-safe? :integer state)
		    (is-graph? (let [loc (top-item :auxiliary state)]
				 (loop [n (mod (top-item :integer state)
					       (* (count (edges loc))
						  (count (nodes loc))))
					loc-list (ensure-list loc)]
				   (cond (seq loc-list) '()
					 (< n (count loc-list)) (nth loc-list n)
					 :else (recur (- n (count loc-list))
						      (next-generator loc-list)))))))]
      (push-item g :auxiliary (pop-item :integer state))
      state)))
      
				      
;; add completely new node, but do not move the graph
(define-registered add-unconnected-node
  (fn [state]
    (if-let [g (and (n-on-stack? 1 :auxiliary state)
		    (not= :no-stack-item (top-item :auxiliary state))
		    (is-graph? (add-node (top-item :auxiliary state) (gensym "NODE::"))))]
      (push-item g :auxiliary (pop-item :auxiliary state))
      state)))

;;adds a node via epsilon transition and moves the graph to the added location
(define-registered add-connected-node
  (fn [state]
    (let [to (gensym "NODE::")
	  from (top-item :auxiliary state)]
      (if-let [g (and (n-on-stack? 1 :auxiliary state)
		      (not= :no-stack-item from)
		      (is-graph? from)
		      (is-graph? (move-graph (add-edge (add-node from to)
						       (node from)
						       to
						       (edge-test :epsilon)
						       (edge-transition-rule (node from) to :epsilon)
						       :epsilon)
					     to)))]
      (push-item g :auxiliary (pop-item :auxiliary state))
      state))))


;; remove the node that the graph is currently at
(define-registered remove-node
  (fn [state]
    (if-let [g (and (stack-safe? :auxiliary state)
		    (not (contains? #{'start 'accept} (node (top-item :auxiliary state))))
		    (is-graph? (let [loc (top-item :auxiliary state)]
				 (remove-node loc (node loc)))))]
      (push-item g :auxiliary (pop-item :auxiliary state))
      state)))

;; add an edge between the last location and two before where the read value
;; is the last input read and moves the graph to that location
(define-registered add-edge
  (fn [state]
    (if (and (stack-safe? :auxiliary state)
	     (stack-safe? :integer state))
      (if-let [loc (is-graph? (top-item :auxiliary state))]
	(let [i (top-item :integer state)
	      alpha (cons :epsilon alphabet)
	      read (nth alpha (mod i (count alpha)))
	      path (path loc)]
	  (if (< (count path) 2)
	    state
	    (if-let [g (move-graph (add-edge loc
					     (node loc)
					     (second path)
					     (edge-test read)
					     (edge-transition-rule (node loc)
								   (second path)
								   read)
					     read)
				   (second path))]
	      (push-item g :auxiliary  (pop-item :auxiliary state))
	      state))
	  state)
	state)
      state)))

;; remove the last transition
(define-registered remove-edge
  (fn [state]
    (if-let [g (and (n-on-stack? 1 :auxiliary state)
		    (not= :no-stack-item (top-item :auxiliary state))
		    (let [loc (top-item :auxiliary state)]
		      (is-graph? (remove-edge loc (node loc) (first (path loc))))))]
      (push-item g :auxiliary (pop-item :auxiliary state))
      state)))


(define-registered connect-to-accept
  (fn [state]
    (if-let [g (and (stack-safe? :auxiliary state)
		    (stack-safe? :integer state)
		    (is-graph? (let [loc (top-item :auxiliary state)
				     read (nth (cons :epsilon alphabet)
					       (mod (top-item :integer state) (inc (count alphabet))))]
				 (add-edge loc
					   (node loc)
					   'accept
					   (edge-test read)
					   (edge-transition-rule (node loc) 'accept read)
					   read))))]
      (push-item g :auxiliary (pop-item :auxiliary state))
      state)))

;; THIS INSTRUCTION FOR TESTING PURPOSES ONLY
;; it enumerates the search space
(define-registered add-connected-nodes
  (fn [state]
    (if-let [g (and (stack-safe? :auxiliary state)
    		    (is-graph? (loop [loc (top-item :auxiliary state)
    				      remaining-reads (cons :epsilon alphabet)]
				 (let [to (gensym "NODE::")]
				   (if (empty? remaining-reads)
				     loc
				     (recur (add-edge (add-node loc to)
						      (node loc)
						      to
						      (edge-test (first remaining-reads))
						      (edge-transition-rule (node loc) to (first remaining-reads))
						      (first remaining-reads))
					    (rest remaining-reads)))))))]
;;    					  (gensym "NODES::"))))))]
      (push-item g :auxiliary (pop-item :auxiliary state))
      state)))

;;(reset! *verbose* true)
(pushgp :error-function regexp-fitness
	:atom-generators regexp-instructions)

	;; :report-simplifications 0
	;; :final-report-simplifications 0
	;; :reproduction-simplifications 0)
	

