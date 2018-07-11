(ns metaprob.inference-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.sequence :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.inference :refer :all]
            [metaprob.examples.gaussian :refer :all]
            [metaprob.examples.inference-on-gaussian :refer :all]
            [metaprob.builtin-impl :as impl]))

;; These tests are smoke tests, not real tests of the methods - we
;; don't expect to get meaningful results with only 16 samples.  The
;; real tests take too long for 'lein test' which I would like to be
;; fast (so it can be run frequently).
;;
;; For actual method tests, we use a longer-running procedure (see
;; long_test.clj).

(def small-nsamples 24)
(def small-nbins 4)
(def weak-threshold 0.5)

;; This is to see whether the test harness itself is basically working:

(deftest check-check
  (testing "check check"
    (let [sampler (fn [i] (uniform 0 1))
          pdf (fn [x] 1)]
      (is (assay "0" sampler small-nsamples pdf small-nbins weak-threshold)))))

;; Compare sampling from Gaussian prior to exact PDF of prior:

(deftest check-prior
  (testing "check sampling from gaussian prior"
    (let [sampler (fn [i] (gaussian 0 1))
          pdf prior-density]
      (is (assay "p" sampler small-nsamples pdf small-nbins weak-threshold)))))
      
;; Inference methods

(deftest check-rejection
  (testing "check rejection sampling"
    (let [sampler (fn [i]
                    (gaussian-sample-value
                     (rejection-sampling two-variable-gaussian-model  ; :model-procedure 
                                         []  ; :inputs 
                                         target-trace  ; :target-trace 
                                         0.5)))
          pdf target-density]
      (is (assay "r" sampler small-nsamples pdf small-nbins weak-threshold)))))

(deftest check-importance
  (testing "check importance sampling"
    (let [n-particles 50
          sampler (fn [i]
                    (gaussian-sample-value
                     (importance-resampling two-variable-gaussian-model
                                            []
                                            target-trace
                                            n-particles)))
          pdf target-density]
      (is (assay "i" sampler small-nsamples pdf small-nbins weak-threshold)))))

(deftest check-MH
  (testing "check M-H sampling"
    (let [steps-per-sample 50
          sampler (fn [i]
                    (gaussian-sample-value
                     (lightweight-single-site-MH-sampling two-variable-gaussian-model
                                                          []
                                                          target-trace
                                                          steps-per-sample)))
          pdf target-density]
      (is (assay "m" sampler small-nsamples pdf small-nbins weak-threshold)))))

