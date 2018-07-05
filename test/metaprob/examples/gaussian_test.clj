
(ns metaprob.examples.gaussian-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace]
            [metaprob.builtin :as builtin]
            [metaprob.prelude :as prelude]
            [metaprob.interpreters :as interp]
            [metaprob.examples.gaussian :refer [two-variable-gaussian-model]]))

(deftest smoke-1
  (testing "smoke test"
    (let [n 3]
      (is (= (builtin/length (interp/replicate n two-variable-gaussian-model))
             n)))))

