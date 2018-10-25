(ns metaprob.test-runner
  (:require [cljs.test :as test :include-macros true]
            [metaprob.state-test]))

(defn -main
  [& args]
  (test/run-tests 'metaprob.state-test))
