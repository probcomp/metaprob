(ns metaprob.distributions-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.sequence :refer :all]
            [metaprob.syntax :refer :all]

            [metaprob.builtin-impl :as impl]
            [metaprob.interpreters :refer [infer]]
            [metaprob.distributions :refer :all]))

(defn get-score [proc & inputs]
  (let [sample (apply proc inputs)
        results (infer :procedure proc
                       :inputs inputs
                       :target-trace (trace :value sample)
                       :output-trace? false)]
    (is (tuple? results) results)
    (is (= (count results) 3) results)
    (let [[_ _ score] results]
      (is (number? score) score)
      score)))

(deftest score-0
  (testing "deterministic procedure score smoke tests"
    (is (= (get-score list) 0))
    (is (= (get-score - 7) 0))
    (is (= (get-score (gen [] 17)) 0))
    (is (= (get-score (gen [x] x) 17) 0))))

(deftest flip-1
  (testing "flip smoke tests"
    (let [r (range 100)
          flips (map (fn [i] (flip 0.5)) r)]
      (is (not (every? not flips)))
      (is (not (every? (fn [x] x) flips))))))

(deftest flip-score-1
  (testing "flip score smoke test"
    (is (< (get-score flip 0.5) 0))))

(deftest uniform-1
  (testing "uniform smoke tests"
    (is (> (uniform 0 1) 0))
    (is (< (uniform 0 1) 1))))

(deftest uniform-score-1
  (testing "flip score smoke test"
    (let [score (get-score uniform 0 1)]
      (is (number? score) score)
      (is (> score -0.1)))))

(defn normalize [weights]
  (let [total (apply + weights)]
    (map (fn [x] (/ x total)) weights)))

;; target-distribution is a seq of [value probability]

(defn test-generator [generator target-distribution reps]
  (let [values (map first target-distribution)
        probabilities (map second target-distribution)
        samples (map (fn [x] (generator)) (range reps))
        measured (normalize
                  (map (fn [value]
                         (apply +
                                (map (fn [sample]
                                       (if (= sample value)
                                         1
                                         0))
                                     samples)))
                       values))
        abs (fn [x] (if (< x 0) (- 0 x) x))
        close? (fn [x y]
                 (if (if (= x 0)
                       (= y 0)
                       (< (abs (- (/ y x) 1)) 0.1))
                   true
                   (do (print [x y]) false)))]
    (every? (fn [x] x) (map close? probabilities measured))))

(deftest categorical-1
  (testing "categorical"
    (let [weights (range 10)
          probabilities (normalize weights)]
      (is (test-generator (fn [] (categorical probabilities))
                          (map (fn [i p] [i p])
                               weights
                               probabilities)
                          10000)))))

(deftest log-categorical-1
  (testing "log-categorical"
    (let [weights (range 10)
          probabilities (normalize weights)
          scores (map (fn [p]
                        (if (= p 0)
                          Double/NEGATIVE_INFINITY
                          (impl/log p)))
                      probabilities)]
      (is (test-generator (fn [] (log-categorical scores))
                          (map (fn [i p] [i p])
                               weights
                               probabilities)
                          5000)))))
