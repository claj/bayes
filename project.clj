(defproject bayes "0.1.0-SNAPSHOT"
  :description "A little library of tools exploring Bayesian filtering, Kalman filters, etc..."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.trace "0.7.8"]
                 [net.mikera/vectorz-clj "0.33.0"]
                 ]

  :profiles  {:dev {:dependencies [[org.clojure/data.csv "0.1.3"]
                                   [incanter "1.5.4" :exclusions [incanter/incanter-mongodb]]]
                    }
              }

  :source-paths ["src", "test"])
