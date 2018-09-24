(defproject metaprob "0.1.0-SNAPSHOT"
  :jvm-opts ["-Xss50M"] ; Default stack size is 1MB or less, increase to 50
  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}
  :profiles {:jupyter {:plugins [[lein-jupyter "0.1.16"]]}})
