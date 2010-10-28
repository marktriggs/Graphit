(defproject graphit "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
		 [org.clojars.marktriggs/jfreechart "1.0.13"]]
  :dev-dependencies [[leiningen/lein-swank "1.2.0-SNAPSHOT"]]
  :namespaces :all
  :aot [graphit]
  :main graphit)
