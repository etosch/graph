(ns graph-test
  (:use graph))

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