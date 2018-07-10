(ns metaprob.examples.long-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.sequence :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.inference :refer :all]
            [metaprob.examples.gaussian :refer :all]
            [metaprob.examples.inference-on-gaussian :refer :all]
            [metaprob.builtin-impl :as impl]))

;; These tests are smoke tests, not real tests of the methods - the
;; real tests take too long and make 'lein test' take too long.
;; For method tests, we use a longer-running procedure.

;; VKM requested 1000 samples on 2018-07-06.
(def nsamples 1000)

;; VKM requested 1000 IS particles (!)
(def n-particles 1000)

;; VKM requested 50 MH steps per sample
(def n-mh-steps 50)

;; JAR's choice (this makes for 40 samples per bin)
(def nbins 25)

;; This is to see whether the test harness itself is basically working:

(deftest check-check
  (testing "check check"
    (let [sampler (fn [i] (uniform 0 1))
          pdf (fn [x] 1)]
      (is (assay "0" sampler nsamples pdf nbins 0.5)))))

;; Compare sampling from Gaussian prior to exact PDF of prior:

(deftest check-prior
  (testing "check sampling from gaussian prior"
    (let [sampler (fn [i] (gaussian 0 1))
          pdf prior-density]
      (is (assay "p" sampler nsamples pdf nbins 0.15)))))

(deftest check-prior-failure
  (testing "check sampling from 'wrong' gaussian prior"
    (let [sampler (fn [i] (gaussian 0.5 1.2)) ;wrong gaussian!!
          pdf prior-density]
      (is (> (badness sampler nsamples pdf nbins) 0.1)))))

(deftest check-rejection
  (testing "check rejection sampling"
    (let [n-particles 20
          sampler (fn [i]
                    (gaussian-sample-value 
                     (rejection-sampling two-variable-gaussian-model  ; :model-procedure 
                                         []  ; :inputs 
                                         target-trace  ; :target-trace 
                                         0.5)))
          pdf target-density]
      (is (assay "r" sampler nsamples pdf nbins 0.15)))))

(deftest check-importance
  (testing "check importance sampling"
    (let [sampler (fn [i]
                    (gaussian-sample-value
                     (importance-resampling two-variable-gaussian-model
                                            []
                                            target-trace
                                            n-particles)))
          pdf target-density]
      (is (assay "i" sampler nsamples pdf nbins 0.2)))))

(deftest check-MH
  (testing "check M-H sampling"
    (let [sampler (fn [i]
                    (gaussian-sample-value
                     (lightweight-single-site-MH-sampling two-variable-gaussian-model
                                                          []
                                                          target-trace
                                                          n-mh-steps)))
          pdf target-density]
      (is (assay "m" sampler nsamples pdf nbins 0.2)))))
