;; The file from which this one was derived is ../main.clj

(ns metaprob.mapl2018.main
  (:gen-class)
  (:require [metaprob.mapl2018.inference-on-gaussian :as inf]))

(defn -main []
  (inf/get-samples)
  (inf/rejection-assay)
  (inf/importance-assay)
  ;; (inf/MH-assay)
  )

