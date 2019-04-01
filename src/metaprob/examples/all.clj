;; Kitchen sink namespace.
;; Intended for use with (in-ns 'metaprob.examples.all).

;; Import a gajillion things so that this namespace is fun to use at
;; the REPL.

(ns metaprob.examples.all
  (:refer-clojure :exclude [map replicate apply])
  (:require
    [metaprob.trace :refer :all]
    [metaprob.generative-functions :refer :all]
    [clojure.pprint :refer [pprint]]
    [metaprob.prelude :refer :all]
    [metaprob.distributions :refer :all]
    [metaprob.inference :refer :all]
    [metaprob.autotrace :refer :all]
    [metaprob.examples.flip-n-coins :refer :all]
    [metaprob.examples.earthquake :refer :all :exclude [flip]]
    [metaprob.examples.inference-on-gaussian :refer :all]))
  ;; The following would simplify startup a bit but I'm getting
  ;; an error in `clojure -Atest`
  ;; (:require [clojure.tools.namespace.repl :refer [refresh]])


;; You may prefer to invoke particular demos in the REPL, rather than
;; run them all wholesale

(defn demo
   []
    ;; Coin flips
    ;; (demo-coin-flips)
    (pprint (coin-flips-demo-n-flips 2))
    (pprint (coin-flips-demo-biased 10))    ;; with intervention

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

    (demo-earthquake)

    ;; 2D gaussian
    ;;   Harness is in main.clj.
    ;;   4 calls / plots: prior, rejection, importance, MH
    ;; (demo-gaussian)

    (let [number-of-runs 100]
      (gaussian-prior-samples number-of-runs)
      (rejection-assay number-of-runs)
      (importance-assay 100 number-of-runs)
     ;; (MH-assay number-of-runs)
      ))
