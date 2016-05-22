(defproject ml-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.5.7"]
                 [tesser.core "1.0.1"]
                 [tesser.math "1.0.1"]
                 [expresso "0.2.2-SNAPSHOT"]
                 [criterium "0.4.4"]
                 [net.mikera/core.matrix "0.52.0"]
                 [net.mikera/vectorz-clj "0.44.0"]
                 [iota "1.1.3"] ;; only use when using reducer over data
                 ])
