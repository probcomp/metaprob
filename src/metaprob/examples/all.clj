;; Kitchen sink namespace.
;; Intended for use with (in-ns 'metaprob.examples.all).

;; Import a gajillion things so that this namespace is fun to use at
;; the REPL.

(ns metaprob.examples.all
  (:refer-clojure :only [ns declare in-ns require])
  (:require [metaprob.state :as state])
  (:require [metaprob.trace :as trace])
  (:require [metaprob.sequence :as sequence])
  (:require [metaprob.builtin-impl :as impl])

  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all])
  (:require [metaprob.prelude :refer :all])
  (:require [metaprob.mem :refer :all])
  (:require [metaprob.distributions :refer :all])
  (:require [metaprob.interpreters :refer :all])
  (:require [metaprob.inference :refer :all])

  (:require [metaprob.compositional :as comp])

  (:require [metaprob.examples.flip-n-coins :refer :all])
  (:require [metaprob.examples.earthquake :refer :all])
  (:require [metaprob.examples.inference-on-gaussian :refer :all])
  ;; The following would simplify startup a bit but I'm getting
  ;; an error in `clojure -Atest`
  ;; (:require [clojure.tools.namespace.repl :refer [refresh]])
  )

;; You may prefer to invoke particular demos in the REPL, rather than
;; run them all wholesale

(define demo
  (gen []

    ;; Coin flips
    ;; (demo-coin-flips)
    (coin-flips-demo-n-flips 2)
    (coin-flips-demo-biased 10)    ;; with intervention

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

    (define number-of-runs 100)
    (gaussian-prior-samples number-of-runs)
    (rejection-assay number-of-runs)
    (importance-assay number-of-runs)
    (MH-assay number-of-runs)

    ))
