;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; adds one connected node for each element of the alphabet
;; (define-registered add-connected-nodes
;;   (fn [state]
;;     (if (and (n-on-stack? 1 :auxiliary state)
;; 	     (not= :no-stack-item (top-item :auxiliary state))
;; 	     (is-graph? (top-item :auxiliary state)))
;;       (let [g (loop [loc (top-item :auxiliary state)
;; 		     remaining-reads alphabet
;; 		     to (gensym "NODE::")]
;; 		(if (empty? remaining-reads)
;; 		  loc
;; 		  (recur (add-edge (add-node loc to)
;; 				   (node loc)
;; 				   to
;; 				   (edge-test (first remaining-reads))
;; 				   (edge-transition-rule (node loc) to (first remaining-reads)))
;; 			 (rest remaining-reads)
;; 			 (gensym "NODES::"))))]
;; 	(push-item g :auxiliary (pop-item :auxiliary state)))
;;       state)))


;; (define-registered generate-empty-graph
;;   (fn [state]
;;     (push-item (new-empty-graph) :auxiliary state)))

;; remove a random edge
;; (define-registered remove-random-edge
;;   (fn [state]
;;     (if (and (n-on-stack? 1 :auxiliary state)
;; 	     (not= :no-stack-item (top-item :auxiliary state)))
;;       (let [loc (top-item :auxiliary state)]
;; 	(if (or (= (node loc) 'start)
;; 		(= (node loc) 'accept))
;; 	  state
;; 	  (let [edge (rand-nth (seq (edges loc)))
;; 		g (remove-edge loc (:from edge) (:to edge))]
;; 	    (if (is-graph? g)
;; 	      (push-item g :auxiliary (pop-item :auxiliary state))
;; 	      state))))
;;       state)))


;; add a random edge between two nodes in the graph.
;; (define-registered add-random-edge
;;   (fn [state]
;;     (if (and (n-on-stack? 1 :auxiliary state)
;; 	     (not= :no-stack-item (top-item :auxiliary state)))	     
;;       (let [loc (top-item :auxilary state)
;; 	    read (rand-nth alphabet)
;; 	    from (rand-nth (seq (nodes loc)))
;; 	    to (rand-nth (seq (nodes loc)))]
;; 	(if (and (is-graph? loc)
;; 		 (contains? (map node (if (seq? (next-processor loc read))
;; 					(next-processor loc read)
;; 					(list (next-processor loc read))))
;; 			    to))
;; 	  state
;; 	  (push-item (add-edge loc from to
;; 			       (edge-test read)
;; 			       (edge-transition-rule from to read))
;; 		     :auxiliary
;; 		     (pop-item :auxiliary state))))
;;       state)))

;; remove a random node in the graph
;; (define-registered remove-random-node
;;   (fn [state]
;;     (if (and (n-on-stack? 1 :auxiliary state)
;; 	     (not= :no-stack-item (top-item :auxiliary state)))
;;       (let [loc (top-item :auxiliary state)
;; 	    g (remove-node loc (rand-nth (seq (nodes loc))))]
;; 	(if (and (not= (node loc) 'start)
;; 		 (not= (node loc) 'accept)
;; 		 (is-graph? g))
;; 	  (push-item g :auxiliary (pop-item :auxiliary state))
;; 	  state))
;;       state)))
