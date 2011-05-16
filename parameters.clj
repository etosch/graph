(repeat 100   {:name "a*ab"
	       :command (list "java" "-jar" "graph-1.0.0-SNAPSHOT-standalone.jar" "examples.regexp"
			      (str "{:population-size 1000 :max-generations 1000 :max-points 35}"))})
