(ns metaprob.test-runner
  (:require [cljs.test :as test :include-macros true]
            [metaprob.autotrace]
            [metaprob.code-handlers]
            [metaprob.compositional-test]
            [metaprob.distributions-test]
            [metaprob.expander]
            [metaprob.generative-functions]
            [metaprob.inference-test]
            [metaprob.prelude-test]
            [metaprob.syntax-test]
            [metaprob.trace-test]))

(defn -main
  [& args]
  (test/run-tests 'metaprob.compositional-test
                  'metaprob.distributions-test
                  'metaprob.inference-test
                  'metaprob.prelude-test
                  'metaprob.trace-test
                  'metaprob.syntax-test))
