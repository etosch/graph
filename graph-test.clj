(require 'graph)

(def test-deterministic-regexp
     ^{:doc "1+0+")
     (grpah/fa-graph {"1" {"1" "1+"}
		    "1+" {"1" "1+", "0" "0"}
		    "0" {"0" "0+"}
		    "0+" {"0" "0+"}}
		   #{"0+"}
		   (fn [input] (if (= input "1") "1" 'dead))
		   'dead)