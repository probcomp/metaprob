(ns metaprob.test-runner
  (:require [cljs.test :as test :include-macros true]
            [metaprob.autotrace]
            [metaprob.code-handlers]
            [metaprob.distributions]
            [metaprob.expander]
            [metaprob.generative-functions]
            [metaprob.inference]
            [metaprob.prelude-test]
            [metaprob.syntax-test]
            [metaprob.trace-test]))

(enable-console-print!)

(defn -main
  [& args]
  (test/run-tests 'metaprob.distributions-test
                  'metaprob.prelude-test
                  'metaprob.trace-test
                  'metaprob.syntax-test))
