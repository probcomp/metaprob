;; The file from which this one was derived is ../main.clj

(ns metaprob.mapl2018.main
  (:gen-class)
  (:require [metaprob.mapl2018.inference-on-gaussian :as inf]
            [criterium.core :as crit]
            ))

(def s-to-ns (* 1000 1000 1000)) ; in ns

(defn instrument [fun & args]
  (flush)
  (if true
    (apply fun args)
    (crit/report-result
     (crit/benchmark*
      fun
      {:warmup-jit-period 0
       :samples 1
       :target-execution-time (* 10 s-to-ns)
       :overhead 0
       }))))

(defn -main []
  (print "---- Prior ----\n")
  (instrument inf/get-samples 100)
  ;; Rejection sampling is very slow - 20 seconds per
  (print "---- Rejection ----\n")
  (instrument inf/rejection-assay 100)
  ;; Rejection sampling is very fast
  (print "---- Importance ----\n")
  (inf/importance-assay 100)
  ;; (print "---- MH ----\n")
  (instrument inf/MH-assay 100))

