;; The file from which this one was derived is ../main.clj

(ns metaprob.examples.main
  (:require [metaprob.examples.inference-on-gaussian :as ginf]
            [metaprob.examples.earthquake :as quake]
            [metaprob.examples.long-test :refer [small-nsamples]]
            [clojure.test :refer :all]
            ;[criterium.core :as crit]
            )
  ;; (:gen-class)
  )

(def s-to-ns (* 1000 1000 1000)) ; in ns

(defn instrument [fun & args]
  (flush)
  (if true
    (apply fun args)
;    (crit/report-result
;     (crit/benchmark*
;      (fn [] (apply fun args))
;      {:warmup-jit-period 0
;       :samples 1
;       :target-execution-time (* 10 s-to-ns)
;       :overhead 0
;       }))
))

;; For a more serious test, try 100 (takes about an hour?)
(def gaussian-number-of-samples 5)
(def quake-number-of-samples 5)

(def n-particles 20)
(def mh-count 20)

(defn -main [& args]
  (letfn [(combine [arg dict]
            (case arg
              "test" (assoc dict :test true)
              "rejection" (assoc dict :rejection true)
              "importance" (assoc dict :importance true)
              "mh" (assoc dict :mh true)
              "quake-rejection" (assoc dict :quake true)
              (let [matches (re-seq #"^\d+$" arg)]
                (if matches
                  (assoc dict :count (Integer. (first matches)))
                  (assert false ["bad arg" arg])))))
          (reduc [args]    ; I'm sure there's a cool way to do this but it's too
                                        ; hard to slog through the clojure docs
            (if (empty? args)
              {}
              (combine (first args)
                       (reduc (rest args)))))]
    (let [dict (if (empty? args)
                 (list :rejection :importance :mh :quake-rejection)
                 (reduc args))]

      (print (format "dict=%s\n" dict))

      (when (get dict :test)
        (run-tests 'metaprob.examples.long-test))

      (let [quake-number-of-samples (or (get dict :count)
                                        quake-number-of-samples)]
        (when (get dict :quake)
          (print "---- earthquake bayesnet ----\n")
          ;; (quake/demo-earthquake) - doesn't work yet
          (quake/earthquake-histogram "bayesnet samples from rejection sampling"
                                (quake/eq-rejection-assay quake-number-of-samples))))

      (let [gaussian-number-of-samples (or (get dict :count)
                                           gaussian-number-of-samples)]
        (when (or (get dict :rejection) (get dict :importance) (get dict :mh))
          (print "---- Prior ----\n")
          (ginf/gaussian-histogram
           "samples from the gaussian demo prior"
           (instrument ginf/gaussian-prior-samples gaussian-number-of-samples)))

        (when (get dict :rejection)
          ;; Rejection sampling is very slow - 20 seconds per
          (print "---- Rejection ----\n")
          (ginf/gaussian-histogram
           "samples from the gaussian demo target"   
           (instrument ginf/rejection-assay gaussian-number-of-samples)))

        (when (get dict :importance)
          ;; Importance sampling is very fast
          (print "---- Importance ----\n")
          (ginf/gaussian-histogram
           (format "importance sampling gaussian demo with %s particles" n-particles)
           (instrument ginf/importance-assay n-particles gaussian-number-of-samples)))

        (when (get dict :mh)
          ;; MH is fast
          (print "---- MH ----\n")
          (ginf/gaussian-histogram
           (format "samples from gaussian demo lightweight single-site MH with %s iterations"
                   mh-count)
           (instrument ginf/MH-assay mh-count gaussian-number-of-samples)))))))
