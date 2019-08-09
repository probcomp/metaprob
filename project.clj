(defproject metaprob "0.1.0-SNAPSHOT"
  :jvm-opts [
             "-Xss50M" ; See `deps.edn` for an explanation of this setting
             "-Dhttps.protocols=TLSv1.2" ; See https://stackoverflow.com/a/50956622
             ]
  :source-paths ["tutorial/src"]
  :resource-paths ["tutorial/resources"]
  :dependencies [[org.clojure/data.json "0.2.6"]
                 [lein-jupyter "0.1.16"]]
  :plugins [[lein-tools-deps "0.4.3"]
            [lein-jupyter "0.1.16"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]})
