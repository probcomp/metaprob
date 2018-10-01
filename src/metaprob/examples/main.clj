(ns metaprob.examples.main
  (:import [java.io File])
  (:require [clojure.test :as test]
            [clojure.tools.cli :as cli]
            [metaprob.examples.inference-on-gaussian :as ginf]
            [metaprob.examples.earthquake :as quake]
            [metaprob.examples.long-test]))

;; The file from which this one was derived is ../main.clj

(defn- s-to-ns
  "Takes a number of seconds `n` and returns the equivalent number of
  nanoseconds."
  [n]
  (* n 1000 1000 1000))

(defn- instrument [fun & args]
  (flush)
  (if true
    (apply fun args)
    (comment
      (crit/report-result
       (crit/benchmark*
        #(apply fun args)
        {:warmup-jit-period 0
         :samples 1
         :target-execution-time (s-to-ns 10)
         :overhead 0})))))

(defn- parse-int
  "Parse an integer from a string."
  [x]
  (Integer/parseInt x))

(defn- greater-than
  "Returns a function to be used with `clojure.tools.cli/parse-opts`'s `:validate`
  option. Returns a validation setting that enforces that the parsed value be
  greater than `n`."
  [n]
  [#(< 1 n) (format "Must be greater than %d" n)])

(def any-of
  "Returns a function to be used with `clojure.tools.cli/parse-opts`'s
  `:default-fn` option. If any of the options passed to `any-of` are true then
  the default will be true."
  some-fn)

(def none-of
  "Returns a function to be used with `clojure.tools.cli/parse-opts`'s
  `:default-fn` option. If none of the options passed to `any-of` are true then
  the default will be true."
  (comp complement any-of))

(def cli-options
  [["-a" "--all"        "Run all the examples"                 :default    false]
   ["-r" "--rejection"  "Run the rejection sampling example"   :default-fn :all]
   ["-i" "--importance" "Run the importance sampling example"  :default-fn :all]
   ["-m" "--mh"         "Run the Metropolis Hastings example"  :default-fn :all]
   ["-q" "--quake"      "Run the earthquake bayes net example" :default    false]
   ["-t" "--test"       "Run the long test example"            :default    false]

   ["-p" "--prior" "Run the prior example"
    :default-fn (any-of :rejection :importance :mh)]

   ["-s" "--samples SAMPLES" "Number of samples for all examples"
    :parse-fn parse-int
    :validate (greater-than 1)
    :default 5]
   [nil "--gaussian-samples SAMPLES" "Number of gaussian samples"
    ;; For a more serious test, try 100 (takes about an hour?)
    :default-fn :samples
    :parse-fn parse-int
    :validate (greater-than 1)]
   [nil "--quake-samples SAMPLES" "Number of quake samples"
    :default-fn :samples
    :parse-fn parse-int
    :validate (greater-than 1)]
   [nil "--particles PARTICLES" "Number of particles"
    :default 20
    :parse-fn parse-int
    :validate (greater-than 0)]
   [nil "--mh-count COUNT" "Metropolis Hastings count"
    :default 20
    :parse-fn parse-int
    :validate (greater-than 0)]

   ["-H" "--help" "Display this help message"
    :default-fn (none-of :rejection :importance :mh :quake :test :prior)]])

(defn- print-help
  [summary]
  (println "\nUSAGE:\n")
  (println "clojure -m" (namespace `print-help) "<options>\n")
  (println summary))

(defn- print-header
  [header]
  (println (format "---- %s ----" header)))

(defn -main
  "Runs examples and outputs samples and commands to `results/`. For a list of
  available options and their defaults see `clojure.tools.cli/parse-opts` and
  `cli-options`."
  [& args]
  (.mkdir (File. "results"))
  (let [{:keys [options summary]}
        (cli/parse-opts args cli-options)]
    (if (:help options)
      (print-help summary)
      (let [{:keys [mh-count particles samples gaussian-samples quake-samples]}
            options]
        (when (:test options)
          (test/run-tests 'metaprob.examples.long-test))

        (when (:prior options)
          (print-header "Prior")
          (ginf/gaussian-histogram
           "samples from the gaussian demo prior"
           (instrument ginf/gaussian-prior-samples gaussian-samples)))

        (when (:rejection options)
          ;; Rejection sampling is very slow - 20 seconds per
          (print-header "Rejection")
          (ginf/gaussian-histogram
           "samples from the gaussian demo target"
           (instrument ginf/rejection-assay gaussian-samples)))

        (when (:importance options)
          ;; Importance sampling is very fast
          (print-header "Importance")
          (ginf/gaussian-histogram
           (format "importance sampling gaussian demo with %s particles" particles)
           (instrument ginf/importance-assay particles gaussian-samples)))

        (when (:mh options)
          ;; MH is fast
          (print-header "Metropolis Hastings")
          (ginf/gaussian-histogram
           (format "samples from gaussian demo lightweight single-site MH with %s iterations"
                   mh-count)
           (instrument ginf/MH-assay mh-count gaussian-samples)))

        (when (:quake options)
          (print-header "Earthquake Bayesnet")
          ;; (quake/demo-earthquake) - doesn't work yet
          (quake/earthquake-histogram
           "bayesnet samples from rejection sampling"
           (quake/eq-rejection-assay quake-samples)))))))
