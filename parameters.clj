(flatten
 ;; regexp run
 (for [regexp '("a*ab" "(aa|bb)*+(c*d)" "(a|s)((d|f)gg*)|k")
       max-steps '(100 1000 10000)]
   (repeat 10  {:name (str regexp "-" max-steps (System/currentTimeMillis))
		:command (list "java" "-Xmx3g" "-jar" "graph-1.0.1-SNAPSHOT-standalone.jar" "examples.regexp"
			       (str "{:population-size 1000 :max-generations 1000 :max-points 35 :regexp " regexp " :max-steps " max-steps "}"))})))