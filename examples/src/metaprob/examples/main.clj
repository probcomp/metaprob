(ns metaprob.examples.main
  (:import [java.io File])
  (:require [clojure.test :as test]
            [clojure.tools.cli :as cli]
            [metaprob.examples.inference-on-gaussian :as ginf]
            [metaprob.examples.earthquake :as quake]
            [metaprob.examples.long-test]))

;; The file from which this one was derived is ../main.clj

(def s-to-ns (* 1000 1000 1000)) ; in ns

(defn instrument [fun & args]
  (flush)
  (if true
    (apply fun args)
    (comment
      (crit/report-result
       (crit/benchmark*
        #(apply fun args)
        {:warmup-jit-period 0
         :samples 1
         :target-execution-time (* 10 s-to-ns)
         :overhead 0})))))

(def cli-options
  [["-a" "--all"        "Run all the examples"                :default false]
   ["-r" "--rejection"  "Run the rejectionsampling example"   :default false]
   ["-i" "--importance" "Run the importance sampling example" :default false]
   ["-m" "--mh"         "Run the Metropolis Hastings example" :default false]
   ["-q" "--quake"      "Run the earthquake bayes net example":default false]
   ["-t" "--test"       "Run the long test example"           :default false]

   ["-s" "--samples SAMPLES" "Number of samples for all examples"
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 %) "Must be greater than 1"]]

   [nil "--quake-samples SAMPLES" "Number of quake samples"
    :default 5
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 %) "Must be greater than 1"]]

   [nil "--gaussian-samples SAMPLES" "Number of gaussian samples"
    ;; For a more serious test, try 100 (takes about an hour?)
    :default 5
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 %) "Must be greater than 1"]]

   [nil "--particles PARTICLES" "Number of particles"
    :default 20
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 %) "Must be positive"]]

   [nil "--mh-count COUNT" "Metropolis Hastings count"
    :default 20
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 %) "Must be positive"]]

   ["-h" "--help" nil]])

(defn print-header
  [header]
  (println (format "---- %s ----" header)))

(defn -main [& args]
  (.mkdir (File. "results"))
  (let [{:keys [options arguments summary] :as opts}
        (cli/parse-opts args cli-options)

        {:keys [all
                rejection importance mh quake test
                samples quake-samples gaussian-samples particles mh-count
                help]}
        options

        all
        (or all (every? false? [rejection importance mh quake test]))]

    (if help
      (println summary)
      #_
      (do (clojure.pprint/pprint options))
      (do
        (when (or all test)
          (test/run-tests 'metaprob.examples.long-test))

        (when (some true? [all rejection importance mh])
          (print-header "Prior")
          (ginf/gaussian-histogram
           "samples from the gaussian demo prior"
           (instrument ginf/gaussian-prior-samples (or samples gaussian-samples))))

        (when (or all rejection)
          ;; Rejection sampling is very slow - 20 seconds per
          (print-header "Rejection")
          (ginf/gaussian-histogram
           "samples from the gaussian demo target"
           (instrument ginf/rejection-assay gaussian-samples)))

        (when (or all importance)
          ;; Importance sampling is very fast
          (print-header "Importance")
          (ginf/gaussian-histogram
           (println (format "importance sampling gaussian demo with %s particles" particles))
           (instrument ginf/importance-assay particles (or samples gaussian-samples))))

        (when (or all mh)
          ;; MH is fast
          (print-header "Metropolis Hastings")
          (ginf/gaussian-histogram
           (format "samples from gaussian demo lightweight single-site MH with %s iterations"
                   mh-count)
           (instrument ginf/MH-assay mh-count (or samples gaussian-samples))))

        (when (or all quake)
          (print-header "Earthquake Bayesnet")
          ;; (quake/demo-earthquake) - doesn't work yet
          (quake/earthquake-histogram
           "bayesnet samples from rejection sampling"
           (quake/eq-rejection-assay (or samples quake-samples))))))))
