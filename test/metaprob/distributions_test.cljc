(ns metaprob.distributions-test
  (:refer-clojure :exclude [apply map replicate])
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]
            [metaprob.prelude :as prelude :refer [apply map replicate]]
            [metaprob.generative-functions :as gen :refer [gen]]
            [metaprob.distributions :as dist]))

(defn get-score
  [proc & inputs]
  (let [[_ tr _]
        (prelude/infer-and-score :procedure proc :inputs inputs)
        [_ _ score]
        (prelude/infer-and-score :procedure proc :inputs inputs :observation-trace tr)]
    score))

(deftest flip-1
  (testing "flip smoke tests"
    (let [r (range 100)
          flips (map (fn [i] (dist/flip 0.5)) r)]
      (is (not (every? not flips)))
      (is (not (every? (fn [x] x) flips))))))

(deftest flip-score-1
  (testing "flip score smoke test"
    (is (< (get-score dist/flip 0.5) 0))))

(deftest uniform-1
  (testing "uniform smoke tests"
    (is (> (dist/uniform 0 1) 0))
    (is (< (dist/uniform 0 1) 1))))

(deftest uniform-score-1
  (testing "flip score smoke test"
    (let [score (get-score dist/uniform 0 1)]
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
    (every? (fn [x] x) (clojure.core/map close? probabilities measured))))

(deftest categorical-1
  (testing "categorical with normalized probabilities"
    (let [weights (range 10)
          probabilities (normalize weights)]
      (is (test-generator (fn [] (dist/categorical probabilities))
                          (clojure.core/map (fn [i p] [i p])
                                            weights
                                            probabilities)
                          100000)))))


(deftest categorical-2
  (testing "categorical with unnormalized probabilities"
    (let [weights (range 10)
          probabilities (normalize weights)]
      (is (test-generator (fn [] (dist/categorical weights))
                          (clojure.core/map (fn [i p] [i p])
                                            weights
                                            probabilities)
                          100000)))))


(deftest categorical-3
  (testing "categorical"
    (let [weights (range 10)
          probabilities (normalize weights)]
      (is (test-generator (fn [] (dist/categorical (zipmap (range 10)
                                                           probabilities)))
                          (clojure.core/map (fn [i p] [i p])
                                            weights
                                            probabilities)
                          100000)))))


(deftest categorical-4
  (testing "categorical"
    (let [weights (range 10)
          probabilities (normalize weights)]
      (is (test-generator (fn [] (dist/categorical (zipmap (range 10) weights)))
                          (clojure.core/map (fn [i p] [i p])
                                            weights
                                            probabilities)
                          100000)))))


(deftest log-categorical-1
  (testing "log-categorical"
    (let [weights (range 10)
          probabilities (normalize weights)
          scores (map (fn [p]
                        (if (= p 0)
                          prelude/negative-infinity
                          (prelude/log p)))
                      probabilities)]
      (is (test-generator (fn [] (dist/log-categorical scores))
                          (clojure.core/map (fn [i p] [i p])
                                            weights
                                            probabilities)
                          100000)))))


(deftest log-categorical-2
  (testing "log-categorical"
    (let [weights (range 10)
          probabilities (normalize weights)
          scores (map (fn [p]
                        (if (= p 0)
                          prelude/negative-infinity
                          (prelude/log p)))
                      probabilities)]
      (is (test-generator (fn [] (dist/log-categorical (zipmap (range 10) scores)))
                          (clojure.core/map (fn [i p] [i p])
                                            weights
                                            probabilities)
                          100000)))))

(deftest log-categorical-small-weights
  (testing "log-categorical with very small probabilities (regression test for
           precision loss issue
           https://github.com/probcomp/metabprob/issues/157)"
    (let [scores '(-746 -745)
          weights (map prelude/exp scores)
          naive-normalized-weights (normalize weights)
          samples (into #{} (repeatedly 1000 #(dist/log-categorical scores)))]
      (is (and
            ;; Normalizing in weight space (not log-weight space aka "score"
            ;; space) leads to one of the weights being exactly zero due to
            ;; severe precision loss.
            (== 0.0 (first naive-normalized-weights))
            ;; As described in https://github.com/probcomp/metabprob/issues/157,
            ;; this condition can fail for two reasons:
            ;; 1) precision loss that causes a loop to run too long and
            ;;    ultimately produces an index-out-of-bounds error,
            ;; 2) precision loss that causes the categorical distribution to
            ;;    assign zero probability mass to the value `0`; the weight
            ;;    should instead be 1/11
            (contains? samples 0))))))
