(ns metaprob.intervention
  (:refer-clojure :exclude [apply get contains? dissoc assoc empty? keys get-in map replicate reduce])
  (:require [clojure.pprint :as pprint]
            [metaprob.builtin :refer :all]
            [metaprob.compound :refer :all]
            [metaprob.code-handlers :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.syntax :refer :all]))

(defn foreign-fn?
  [f]
  (and (not (get f :generative-source))
       (not (get f :implementation))))

(def intervene
  (gen [model intervention-trace]
    (cond
      ;; If no interventions, do nothing
      (empty? intervention-trace)
      model

      ;; If intervening on `apply`, produce a function that
      ;; applies the intervened procedure to the arguments
      (get model :apply?)
      (gen [proc args]
        (apply (intervene proc intervention-trace) args))

      ;; Base case: a primitive turns into a function that
      ;; always traces the same value.
      (primitive? model)
      (gen {:tracing-with t} [& args]
        (t '() exactly (trace-value intervention-trace)))

      ;; Intervening on a foreign function's return value.
      ;; Run the foreign function for side effects, and return
      ;; the intervened value.
      (foreign-fn? model)
      (gen [& args] (apply model args)
        (trace-value intervention-trace))

      ;; If we have a model (and perhaps an implementation),
      ;; strip the implementation and return the intervened model.
      (get model :model)
      (recur (get model :model) intervention-trace)

      ;; Otherwise, we have ordinary generative code.
      ;; There are two ways we could try to do intervention.
      ;; One way actually modifies the code to find the places
      ;; where a tracer is being called, and manually edits those all
      ;; to either (a) be calls to an intervened version of the procedure,
      ;; or (b) be calls to `exactly` with the intervened value.
      ;; I think it's worth writing that version up at some point. But
      ;; this version is Way #2, and is easier to implement:
      ;; We run the procedure, intercepting calls to its tracer and
      ;; converting them into calls to the intervened version.
      true
      (gen {:tracing-with t} [& args]
        (let [tracer
              (gen [addr proc & ins]
                (t addr apply
                   (intervene proc
                              (maybe-subtrace intervention-trace addr))
                   ins))]
          (apply ((get model :clojure-impl) tracer) args))))))