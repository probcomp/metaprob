
(ns metaprob.oopsla.inference-on-gaussian-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace]
            [metaprob.builtin :as builtin]
            [metaprob.prelude :as prelude]
            [metaprob.oopsla.gaussian :refer [two-variable-gaussian-model]]
            [metaprob.oopsla.inference-on-gaussian :refer :all]))

(deftest smoke-1
  (testing "smoke test"
    (let [n 3]
      (is (= (builtin/length (prelude/replicate n two-variable-gaussian-model))
             n)))))

