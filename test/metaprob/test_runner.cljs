(ns metaprob.test-runner
  (:require [cljs.test :as test :include-macros true]
            [metaprob.all-tests]
            [metaprob.autotrace]
            [metaprob.code-handlers]
            [metaprob.expander]
            [metaprob.generative-functions]))

(defn -main
  [& args]
  (test/run-tests 'metaprob.compositional-test
                  'metaprob.distributions-test
                  'metaprob.inference-test
                  'metaprob.prelude-test
                  'metaprob.trace-test
                  'metaprob.syntax-test))
