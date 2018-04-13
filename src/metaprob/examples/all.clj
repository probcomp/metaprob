;; Kitchen sink namespace.
;; Intended for use with (in-ns 'metaprob.examples.all).

(ns metaprob.examples.all
  (:refer-clojure :only [ns declare in-ns])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.examples.flip-n-coins :refer :all]
            [metaprob.examples.earthquake :refer :all]
            [metaprob.examples.inference-on-gaussian :all]
            [metaprob.examples.interpreters :refer :all]))

;; Coin flips
;;   (coin-flips-demo-2-flips)
;;   (coin-flips-demo-biased)  - with intervention

;; Bayes net (earthquake)
;;   prior:
;;     exact probabilities
;;     random sample
;;   with intervention:
;;     exact probabilities
;;     random sample
;;   TBD: importance sampling
;;   TBD: rejection resampling
;;   earthquake_bayesian_network could return the query instead of
;;     the trace (that would then be queried)

;; 2D gaussian
;;   Harness is in main.clj.
;;   4 calls / plots: prior, rejection, importance, MH

;;   The inference utilities all return output traces.
;;   But the returned value is of interest - in the case of gaussian,
;;    that's really a better way to access the sample.
