
(ns metaprob.oopsla.inference-on-gaussian-test
  (:require [clojure.test :refer :all]
            [metaprob.oopsla.inference-on-gaussian :refer :all]))

(deftest smoke-1
  (testing "smoke test"
    (is (> number-of-runs 0))))
