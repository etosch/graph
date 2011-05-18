;; regexp.clj
(ns examples.regexp
  (:gen-class)
  (:require [clojure.set :as set])
  (:use [runner]
	[clojush :exclude [ensure-list rcons]]
	[graph :exclude 'add-edge]
	[graph-utils.fa :exclude '*verbose*]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modification clojush to print the best graph
(in-ns 'clojush)
(use 'graph)
(reset! global-error-reuse false)
(def best-graph (atom '()))
       
(defn graph-display [loc]
  (when (seq loc)
    (list loc
	  {:nodes (nodes loc)
	   :edges (map #(select-keys % '(:from :to :read)) (edges loc))
	   :accept-nodes (map node (filter #((accept-fn loc) %) (map #(move-graph loc %) (nodes loc))))})))

(defn problem-specific-report
  [best population generation error-function report-simplifications]
  (println "\nBest graph:"
	   (graph-display (:graph (aux/find-first
				   #(and (= (:code %)(:program best))
					 (= (ensure-list (:error %))
					    (:errors best)))
				   @best-graph)))))

(defn stack-safe? [type state & [n]]
  (and (n-on-stack? (or n 1) type state)
       (not-any? #(or (= :no-stack-item %)
		      (nil? %)
		      (and (coll? %) (empty? %)))
		 (take (or n 1) (type state)))))

(defn safe-top-item [type state]
  (and (stack-safe? type state) (top-item type state)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-ns 'examples.regexp)
(use 'clojush)
(use 'graph-utils.fa)

(def regexp
     (atom 
      (fa-graph {'a* {"a" ['a* 'a*a]}
		 'a*a {"b" 'accept}}
		{'start {"a" ['a* 'a*a] "b" 'dead}}
		#{'accept}
		nil)))
(def alphabet
     (atom '("a" "b")))
(def first-20-regexps (doall (enumerate-strings @regexp 20)))

(defn is-graph? [loc]
  (and (vector? loc) (symbol? (first loc)) (map? (loc 1)) (meta loc) loc))

(defn eval-machine [char-list loc]
  (if (is-graph? loc)
    (deref (evaluate (with-input (move-graph loc 'start) char-list)))
    (do (println "loc is not a graph: " loc)
	:reject)))

(defn new-empty-graph []
  (fa-graph {'start {:epsilon 'accept}} {} #{'accept} nil))

(defn edge-exists? [loc from to read]
  (contains? (set (map #(select-keys % '(:from :to :read)) (edges loc)))
	     {:read read :from from :to to}))

(defn regexp-error
  [program]
  (let [state (run-push program (push-item (new-empty-graph) :auxiliary (make-push-state)))
	evolved-graph (top-item :auxiliary state)
	regexps-1 (take 10 (shuffle first-20-regexps))
	regexps-2 (doall (enumerate-strings evolved-graph 15))
	;; penalize epsilon transitions
	fit-0 (count (filter #(= (:read %) :epsilon) (edges evolved-graph)))
	;; penalize dead states
	fit-1 (count (set/difference (disj (nodes evolved-graph) 'accept)
				     (set (map :from (edges evolved-graph)))))
	;; penalize when the evolved machine rejects the teacher's output
	fit-2 (- 10 (count (filter #(= :accept %) (pmap #(eval-machine % evolved-graph) regexps-1))))
	;; penalize when the teacher rejects the learners output
	fit-3 (- 15 (count (filter #(= :accept %) (pmap #(eval-machine % @regexp) regexps-2))))
 	fitness (+ fit-0 fit-1 fit-2)]
    ;;    (println fit-0 fit-1 fit-2)
;;    (println regexps-2)
    (cond
     ;; if the best graph is empty or the current error is better
     ;; than the current set, reset
     (or (empty? @best-graph) (> (:error (first @best-graph)) fitness))
     (reset! best-graph
	     (list {:error fitness :code program
		    :graph (top-item :auxiliary state)}))
     ;; if the current error is the same as the current set, conj
     (= (:error (first @best-graph)) fitness)
     (swap! best-graph conj 
	    {:error fitness :code program
	     :graph (top-item :auxiliary state)}))
    (list fitness)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push graph instructions

;; moves the graph to the start node
(define-registered start
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  g (is-graph? (move-graph top-aux 'start))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))

;; moves the graph to the accept node
(define-registered accept
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  g (is-graph? (move-graph top-aux 'accept))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))

;; move to the nth child of the current node
(define-registered nth-next
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  top-int (safe-top-item :integer state)
		  next-locs (aux/return-if-all (ensure-list (next-generator top-aux)) #(> (count %) 0))
		  g (is-graph? (nth next-locs (mod top-int (count next-locs))))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))


;; move the graph back to the last valid location
(define-registered prev
  (fn [state]
    (aux/if-let* [top-aux (is-graph? (safe-top-item :auxiliary state))
		  g (is-graph? (prev top-aux))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))

;; jump to the nth added node (leverages gensym's lexicographic ordering)
(define-registered nth-node
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  top-int (safe-top-item :integer state)
		  g (is-graph? (move-graph top-aux
					   (nth (sort (nodes top-aux))
						(mod top-int (count (nodes top-aux))))))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))
      
				      
;; add completely new node, but do not move the graph
(define-registered add-unconnected-node
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  g (is-graph? (add-node top-aux (gensym "NODE::")))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))

;;adds a node moves the graph to the added location
(define-registered add-connected-node
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  top-int (safe-top-item :integer state)
		  read (nth (cons :epsilon @alphabet)
			    (mod top-int (inc (count @alphabet))))
		  to (gensym "NODE::")
		  g (is-graph? (move-graph (add-edge (add-node top-aux to)
						 (node top-aux)
						 to
						 (edge-test read)
						 (edge-transition-rule (node top-aux) to read)
						 read)
					   to))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))


;; remove the node that the graph is currently at
;; chose here instead of prev to counterbalance presence of start and accept
(define-registered remove-node
  (fn [state]
    (aux/if-let* [top-aux (aux/return-if-all (safe-top-item :auxiliary state)
					     #(not (contains? #{'start 'accept} (node %))))
		  g (is-graph? (remove-node top-aux (node top-aux)))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))
  
;; add an edge between the current location and two before where the read value
;; is the last input read and moves the graph to that location
(define-registered add-edge
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  top-int (safe-top-item :integer state)
		  alpha (cons :epsilon @alphabet)
		  read (nth alpha (mod top-int (count alpha)))
		  path (and (> (count (path top-aux)) 2) (path top-aux))
		  g (is-graph?
		     (when-not (edge-exists? top-aux (node top-aux) (nth path 2) read)
		       (move-graph (add-edge top-aux
					     (node top-aux)
					     (nth path 2)
					     (edge-test read)
					     (edge-transition-rule (node top-aux) (second path) read)
					     read)
				   (nth path 2))))]
		 (push-item g :auxiliary  (pop-item :auxiliary state))
		 state)))

;; analogue to nth-node; adds an edge between the current node and the nth connected node.
;; Does not move.
(define-registered add-nth-edge
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  top-int (safe-top-item :integer state)
		  alpha (cons :epsilon @alphabet)
		  read (nth alpha (mod top-int (count alpha)))
		  to (nth (sort (nodes top-aux)) (mod top-int (count (nodes top-aux))))
		  g (is-graph?
		     (when-not (edge-exists? top-aux (node top-aux) to read)
		       (add-edge top-aux
				 (node top-aux)
				 to
				 (edge-test read)
				 (edge-transition-rule (node top-aux) to read)
				 read)))]
		  (push-item g :auxiliary (pop-item :auxiliary state))
		  state)))

;; Removes the edge between the current node and the previously visited node. Stays at the current node.
(define-registered remove-edge
  (fn [state]
    (aux/if-let* [top-aux (aux/return-if-all (safe-top-item :auxiliary state)
					     is-graph? #(first (path %)))
		  g (is-graph? (remove-edge top-aux (second (path top-aux)) (first (path top-aux))))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))

;; Connects the the node sitting atop the auxiliary stack to the accept node. Does not move the node;
;; just adds an edge whose transition function is a function of the @alphabet plus :epsilon
(define-registered connect-to-accept
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  top-int (safe-top-item :integer state)
		  g (-> (let [loc top-aux
			      read (nth (cons :epsilon @alphabet) (mod top-int (inc (count @alphabet))))]
		      (when-not (some #(= % {:read read :to 'accept :from (node loc)})
				      (map #(select-keys % (list :from :to :read)) (edges loc)))
			(add-edge loc
				  (node loc)
				  'accept
				  (edge-test read)
				  (edge-transition-rule (node loc) 'accept read)
				  read)))
			(is-graph?))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))

;; THIS INSTRUCTION FOR TESTING PURPOSES ONLY
;; it enumerates the search space
(define-registered add-connected-nodes
  (fn [state]
    (aux/if-let* [top-aux (safe-top-item :auxiliary state)
		  g (loop [loc top-aux remaining-reads (cons :epsilon @alphabet)]
		      (let [from (node loc)
			    to (gensym "NODE::")
			    read (first remaining-reads)]
			(cond (nil? read) loc
			      (contains? (set (map :read (edges loc))) read) (recur loc (rest remaining-reads))
			      :else (recur (add-edge (add-edge (add-node loc to) from to
							       (edge-test read)
							       (edge-transition-rule from to read)
							       read)
						     to 'accept (edge-test :epsilon) (edge-transition-rule to 'accept :epsilon) :epsilon)
					   (rest remaining-reads)))))]
		 (push-item g :auxiliary (pop-item :auxiliary state))
		 state)))

(def regexp-instructions (cons (fn [] (rand-int 50))
			       '(start accept
				       nth-next
				       prev
				       nth-node
				       add-unconnected-node
				       add-connected-node
				       remove-node
				       add-edge
				       add-nth-edge
				       remove-edge ;;add-connected-nodes
				       connect-to-accept)))
#_(do (reset! best-graph '())
      (reset! max-steps 1000)
      (reset! regexp (regexp-to-nfa "(a+b)c*"))
      (reset! alphabet '("a" "b" "c"))
      (pushgp :error-function regexp-error
	      :atom-generators regexp-instructions
	      :max-generations 1000
	      :max-points 50
	      :population-size 250
	      :report-simplifications 0
	      :final-report-simplifications 0
	      :reproduction-simplifications 0))


;; (defn -main [regexp-string max-gens max-points pop-size max-eval-steps]
;;   (println regexp max-gens max-points pop-size max-eval-steps)
;;   (reset! regexp (regexp-to-nfa regexp-string))
;;   (reset! alphabet (filter #(not (contains? #{"(" ")" "*" "+" "|"} %)) (map str regexp-string)))w
;;   (reset! max-steps (read-string max-eval-steps))
;;   (pushgp :error-function regexp-error
;; 	  :atom-generators regexp-instructions
;; 	  :max-generations (read-string max-gens)
;; 	  :max-points (read-string max-points)
;; 	  :population-size (read-string pop-size)))

(in-ns 'runner)
(use 'clojush)
(use 'examples.regexp)
;;(reset! *verbose* true)

(defn run [params]
  (let [max-gen (params :max-generations)
	max-points (params :max-points)
	pop-size (params :population-size)
	regexp (params :regexp)
	max-steps (params :max-steps)
	instruction-set (params :instruction-set)
	xover-p (params :xover-p)
	mutation-p (params :mutation-p)]
    (try 
      (do (reset! best-graph '())
	  (when regexp
	    (reset! regexp (graph-utils.fa/regexp-to-nfa regexp))
	    (reset! alphabet (filter #(not (contains? #{"(" ")" "*" "+" "|"} %)) (map str regexp)))
	    (pushgp :error-function regexp-error
		    :atom-generators (or instruction-set regexp-instructions)
		    :max-generations max-gen
		    :max-points max-points
		    :population-size pop-size
		    :mutation-probability (or mutation-p 0.45)
		    :crossover-probability (or xover-p 0.45)
		    :report-simplifications 10)))
      (catch Exception e :error))))

