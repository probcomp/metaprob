(ns metaprob.test-runner
  (:require [cljs.test :as test :include-macros true]
            [metaprob.sequence-test]
            [metaprob.state-test]
            [metaprob.trace-test]))

(defn -main
  [& args]
  (test/run-tests 'metaprob.sequence-test
                  'metaprob.state-test
                  'metaprob.trace-test))
