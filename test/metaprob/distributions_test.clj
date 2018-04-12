(ns metaprob.distributions-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace]
            [metaprob.builtin-impl :as impl]
            [metaprob.syntax :refer :all]
            [metaprob.infer :refer [infer-apply]]
            [metaprob.distributions :refer :all]))

(defn get-score [proc inputs]
  (let [target (trace/trace :value (apply proc inputs))
        [answer score] (infer-apply proc inputs nil target nil)]
    score))

(deftest score-0
  (testing "deterministic procedure score smoke tests"
    (is (= (get-score list (list)) 0))
    (is (= (get-score - (list 7)) 0))
    (is (= (get-score (gen [] 17) (list )) 0))
    (is (= (get-score (gen [x] x) (list 17)) 0))))

(deftest flip-1
  (testing "flip smoke tests"
    (let [r (range 100)
          flips (map (fn [i] (flip 0.5)) r)]
      (is (not (every? not flips)))
      (is (not (every? (fn [x] x) flips))))))

(deftest flip-score-1
  (testing "flip score smoke test"
    (is (< (get-score flip (list 0.5)) 0))))

(deftest uniform-1
  (testing "uniform smoke tests"
    (is (> (uniform 0 1) 0))
    (is (< (uniform 0 1) 1))))

(deftest uniform-score-1
  (testing "flip score smoke test"
    (is (> (get-score uniform (list 0 1)) -0.1))))

