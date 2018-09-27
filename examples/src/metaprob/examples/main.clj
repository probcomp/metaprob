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
  [["-a" "--all"        "Run all the examples"                 :default    false]
   ["-r" "--rejection"  "Run the rejection sampling example"   :default-fn :all]
   ["-i" "--importance" "Run the importance sampling example"  :default-fn :all]
   ["-m" "--mh"         "Run the Metropolis Hastings example"  :default-fn :all]
   ["-q" "--quake"      "Run the earthquake bayes net example" :default    false]
   ["-t" "--test"       "Run the long test example"            :default    false]

   ["-s" "--samples SAMPLES" "Number of samples for all examples"
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 %) "Must be greater than 1"]
    :default 5]

   [nil "--gaussian-samples SAMPLES" "Number of gaussian samples"
    ;; For a more serious test, try 100 (takes about an hour?)
    :default-fn :samples
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 %) "Must be greater than 1"]]

   [nil "--quake-samples SAMPLES" "Number of quake samples"
    :default-fn :samples
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

   ["-h" "--help" nil
    :default-fn (complement (some-fn :rejection :importance :mh :quake :test))]])

(defn print-help
  [summary]
  (println "Run Metaprob examples")
  (println)
  (println summary))

(defn print-header
  [header]
  (println (format "---- %s ----" header)))

(defn -main [& args]
  (.mkdir (File. "results"))
  (let [{:keys [options arguments summary] :as opts}
        (cli/parse-opts args cli-options)

        {:keys [rejection importance mh quake test
                quake-samples gaussian-samples particles mh-count
                all help]}
        options]
    (if help
      (print-help summary)
      (do
        (when test
          (test/run-tests 'metaprob.examples.long-test))

        (when (some true? [rejection importance mh])
          (print-header "Prior")
          (ginf/gaussian-histogram
           "samples from the gaussian demo prior"
           (instrument ginf/gaussian-prior-samples gaussian-samples)))

        (when rejection
          ;; Rejection sampling is very slow - 20 seconds per
          (print-header "Rejection")
          (ginf/gaussian-histogram
           "samples from the gaussian demo target"
           (instrument ginf/rejection-assay gaussian-samples)))

        (when importance
          ;; Importance sampling is very fast
          (print-header "Importance")
          (ginf/gaussian-histogram
           (format "importance sampling gaussian demo with %s particles" particles)
           (instrument ginf/importance-assay particles gaussian-samples)))

        (when mh
          ;; MH is fast
          (print-header "Metropolis Hastings")
          (ginf/gaussian-histogram
           (format "samples from gaussian demo lightweight single-site MH with %s iterations"
                   mh-count)
           (instrument ginf/MH-assay mh-count gaussian-samples)))

        (when quake
          (print-header "Earthquake Bayesnet")
          ;; (quake/demo-earthquake) - doesn't work yet
          (quake/earthquake-histogram
           "bayesnet samples from rejection sampling"
           (quake/eq-rejection-assay quake-samples)))))))
