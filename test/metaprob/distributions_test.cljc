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

; (deftest multivariate-gaussian-1
;   (testing "multivariate gaussian smoke tests"
;     (let [mu-1 [0 0]
;           sigma-1 [[1 0] [0 1]]
;           mu-2 [1.2 -0.7]
;           sigma-2 [[2 -0.6][-0.6 0.2]]
;           score-1 (get-score dist/multivariate-gaussian mu-1 sigma-1 [0 0])
;           score-2 (get-score dist/multivariate-gaussian mu-2 sigma-2)]
;       (is (number? score-1) score-1)
;       (is (= (clojure.core.matrix/shape (dist/multivariate-gaussian mu-2 sigma-2)) [1 2])))))

; (deftest multivariate-gaussian-score-1
;   (testing "multivariate gaussian score smoke tests"
;     (let [mu-1 [0 0]
;           sigma-1 [[1 0] [0 1]]
;           mu-2 [1.2 -0.7]
;           sigma-2 [[2 -0.6][-0.6 0.2]]]
;       (is (= (clojure.core.matrix/shape (dist/multivariate-gaussian mu-1 sigma-1)) [1 2]))
;       (is (= (clojure.core.matrix/shape (dist/multivariate-gaussian mu-2 sigma-2)) [1 2])))))

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
