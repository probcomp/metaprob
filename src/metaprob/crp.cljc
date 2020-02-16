(ns metaprob.crp
  (:refer-clojure :exclude [apply map replicate reduce])
  (:require [metaprob.prelude :as mp :refer [map make-primitive]]
            #?(:clj [incanter.distributions :as distributions])))

;; Note: in wikipedia, the parameter that we are calling 'alpha' is
;; called 'theta'.  The 'alpha' in wikipedia is assumed to be zero.

;; A CRP state is a 3-vector
;;  [table-counts customer-count alpha]
;; where table-counts is a vector of the number of customers at eash table,
;; customer-count caches the total number customers,
;; and alpha is the CRP parameter.
;; customer-count is not used at present, because categorical takes
;; care of normalization, so we should probably remove it.

(defn empty-state [alpha]
  [[] 0 alpha])

(def sample
  (gen [state]
    (let [[table-counts customer-count alpha] state]
      ;; Think about this.
      (let [weights (concat table-counts [alpha])]
        (categorical weights)))))

(defn incorporate [state new-seating]
  (let [[table-counts customer-count alpha] state]
    [(if (>= new-seating (len table-counts))
       (concat table-counts [1])
       (assoc table-counts
              new-seating
              (+ (table-counts new-seating) 1)))
     (+ customer-count 1)
     alpha]))
