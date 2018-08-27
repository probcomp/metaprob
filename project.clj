(defproject metaprob "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "GNU General Public License Version 3, 29 June 2007"
            :url "http://www.gnu.org/licenses/"}
  ;; Default stack size is 1MB or less, increase to 50
  :jvm-opts ["-Xss50M"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [kixi/stats "0.4.0"]
                 [criterium "0.4.4"]
                 [org.apache.commons/commons-math3 "3.6.1"]
		 [incanter "1.5.7"]
		 [org.clojure/math.numeric-tower "0.0.4"]
		 [org.clojure/math.combinatorics "0.1.4"]]

  :plugins [[lein-jupyter "0.1.16"]]

  ;; :aot [metaprob.basic-trace]
  )
