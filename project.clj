(defproject bebot "0.1"
  :description "A bot that plays Bejeweled with a very simple AI."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.2.0"]]
  :main bebot.core
  :jvm-opts ["-Xmx1g"])
