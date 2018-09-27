(defproject metaprob "0.1.0-SNAPSHOT"
  :jvm-opts ["-Xss50M"] ; See `deps.edn` for an explanation of this setting
  :plugins [[lein-tools-deps "0.4.1"]
            [lein-jupyter "0.1.16"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]})
