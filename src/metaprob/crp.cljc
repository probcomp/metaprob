(ns metaprob.crp
  (:refer-clojure :exclude [apply map replicate reduce])
  (:require [metaprob.generative-functions :as gen :refer [gen]]
            [metaprob.prelude :as mp :refer [map make-primitive]]
            [metaprob.distributions :as distributions]))

;; Note: in wikipedia, the parameter that we are calling 'alpha' is
;; called 'theta'.  The 'alpha' in wikipedia is assumed to be zero.

;; A CRP state is a 3-vector
;;  [table-counts customer-count alpha]
;; where table-counts is a vector of the number of customers at eash table,
;; customer-count caches the total number customers,
;; and alpha is the CRP parameter.
;; customer-count is not used at present, because categorical takes
;; care of normalization, so we should probably remove it.

(def sample
  (gen [counts alpha]
    (distributions/categorical (concat counts [alpha]))))

(defn incorporate [counts new-seating]
  (if (>= new-seating (count counts))
    (vec (concat counts
                 (repeat (- new-seating (count counts)) 0)
                 [1]))
    (assoc counts
           new-seating
           (+ (counts new-seating) 1))))
