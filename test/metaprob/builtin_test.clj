(ns metaprob.builtin-test
  (:require [clojure.test :refer [deftest testing is]]
            [metaprob.trace :as trace]
            [metaprob.builtin :refer :all])
  (:refer-clojure :exclude [assoc dissoc apply]))

;; Procedure stuff

(deftest sample-1
  (testing "sample-uniform smoke tests"
    (let [x (sample-uniform)
          y (sample-uniform)]
      (is (> x 0))
      (is (< x 1))
      (is (> y 0))
      (is (< y 1))
      (is (not (= x y))))))


(deftest apply-1
  (testing "apply smoke test"
    (is (= (apply - [3 2]) 1))
    (is (= (apply - (list 3 2)) 1))
    (is (= (apply apply (list - (list 3 2))) 1))))
