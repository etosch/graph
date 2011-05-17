(defproject graph "1.0.1-SNAPSHOT"
  :description "Functional, local graph representation."
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :dependencies [[clojush "20110118-kephale.21"]
		 [org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :jvm-opts ["-Xmx1536m"]
  :aot [aux graph graph-utils.fa examples.regexp]
  :main examples.regexp
  :url "http:/github.org/etosch/graph")
;;  "-agentlib:hprof=heap=sites"]
;;  "-javaagent:/home/g/grad/etosch/Downloads/AppDynamicsLite/AppServerAgentLite/javaagent.jar"]
;;  "-agentlib:hprof=dump"] 