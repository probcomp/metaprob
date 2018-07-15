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

;; These tests are smoke tests, not real tests of the methods - the
;; real tests take too long and make 'lein test' take too long.
;; For method tests, we use a longer-running procedure.

(def small-nsamples 16)
(def small-nbins 4)

;; This is to see whether the test harness itself is basically working:

(deftest check-check
  (testing "check check"
    (let [sampler (fn [i] (uniform 0 1))
          pdf (fn [x] 1)]
      (is (assay "0" sampler small-nsamples pdf small-nbins 0.4))    ;0.1812862032169984
      ;; but later we got 0.4325816933252141!
      ;; (is (assay sampler 1000 pdf 30 0.2)) ;0.12964033897259208
      )))

;; Compare sampling from Gaussian prior to exact PDF of prior:

(deftest check-prior
  (testing "check sampling from gaussian prior"
    (let [sampler (fn [i] (gaussian 0 1))
          pdf prior-density]
      (is (assay "p" sampler small-nsamples pdf small-nbins 0.4))      ;0.30480195647613484
      (is (assay "p" sampler 1000 pdf 30 0.15))   ;0.0940586128869054
      ;; (is (assay sampler 10000 pdf 100 0.1)) ;0.07330583691271352
      )))

(deftest check-prior-failure
  (testing "check sampling from 'wrong' gaussian prior"
    (let [sampler (fn [i] (gaussian 0.5 1.2)) ;wrong gaussian!!
          pdf prior-density]
      (is (> (badness sampler small-nsamples pdf small-nbins) 0.1))       ;0.2721880569380256
      ;; (is (> (badness sampler 100 pdf 10) 0.1))       ;0.2721880569380256
      ;; (is (> (badness sampler 1000 pdf 30)) 0.2)   ;0.35269083367370974
      ;; (is (> (badness sampler 10000 pdf 100)) 0.3) ;0.3738335775553201
      )))

(deftest check-rejection
  (testing "check rejection sampling"
    (let [n-particles 20
          sampler (fn [i]
                    (trace-get 
                     (rejection-sampling two-variable-gaussian-model  ; :model-procedure 
                                         []  ; :inputs 
                                         target-trace  ; :target-trace 
                                         0.5)))
          pdf target-density]
      (is (assay "r" sampler small-nsamples pdf small-nbins 0.3)) ;0.0804781006306281
      ;; (is (assay sampler 1000 pdf 30 0.2)) ;0.15326279635446427
      )))

(deftest check-importance
  (testing "check importance sampling"
    (let [n-particles 20
          sampler (fn [i]
                    (trace-get
                     (importance-resampling two-variable-gaussian-model
                                            []
                                            target-trace
                                            n-particles)))
          pdf target-density]
      (is (assay "i" sampler small-nsamples pdf small-nbins 0.3)) ;0.22507013111326224
      ;; (is (assay sampler 1000 pdf 30 1.0)) ;0.20789431155239813
      ;; (is (assay sampler 10000 pdf 100 0.5)) ;0.18844152375579348
      )))

(deftest check-MH
  (testing "check M-H sampling"
    (let [steps-per-sample 50
          sampler (fn [i]
                    (trace-get
                     (lightweight-single-site-MH-sampling two-variable-gaussian-model
                                                          []
                                                          target-trace
                                                          steps-per-sample)))
          pdf target-density]
      (is (assay "m" sampler small-nsamples pdf small-nbins 0.8)) ;0.4144289258883227  ugh.
      ;; later: 0.5355234073375498
;;      (is (< (assay sampler 1000 pdf 30)) 1.0)
;;      (is (< (assay sampler 10000 pdf 100)) 0.5)
      )))