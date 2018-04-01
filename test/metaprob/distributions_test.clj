(ns metaprob.distributions-test
  (:require [clojure.test :refer :all]
            [metaprob.builtin-impl :as builtin]
            [metaprob.syntax :refer :all]
            [metaprob.infer :as infer]
            [metaprob.distributions :refer :all]))

(deftest flip-1
  (testing "flip smoke tests"
    (let [r (range 100)
          flips (map (fn [i] (flip 0.5)) r)]
      (is (not (every? not flips)))
      (is (not (every? (fn [x] x) flips))))))

(defn get-score [proc inputs]
  (let [[answer score] (infer/infer proc inputs nil nil nil)]
    score))

(deftest score-0
  (testing "score smoke test degenerate"
    (is (= (get-score list []) 0))
    (is (= (get-score - [7]) 0))
    (is (= (get-score (gen [] 17) []) 0))
    (is (= (get-score (gen [x] x) [17]) 0))))

(deftest score-1
  (testing "score smoke test"
    (is (< (get-score flip [0.5]) 0))))

