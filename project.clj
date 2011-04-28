(defproject graph "1.0.0-SNAPSHOT"
  :description "Functional, local graph representation."
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :dependencies [[clojush "20110118-kephale.21"]
		 [org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :aot [graph graph-utils.fa]
  :url "http:/github.org/etosch/graph")
