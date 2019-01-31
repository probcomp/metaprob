(ns metaprob.examples.multimix
  (:refer-clojure :only
    [defn for frequencies gensym group-by last let merge nil? pos? zipmap])
  (:require
    [metaprob.trace :as trace]
    [metaprob.builtin-impl :as impl]
    [metaprob.syntax :refer :all]
    [metaprob.builtin :refer :all]
    [metaprob.prelude :refer :all]
    [metaprob.context :refer :all]
    [metaprob.distributions :refer :all]
    [metaprob.interpreters :refer :all]
    [metaprob.inference :refer :all]
    [metaprob.compositional :as comp]
    [metaprob.examples.gaussian :refer :all]))


(define make-view
  (gen [vars-and-dists [cluster-probs cluster-params]]
    (define var-names (keys vars-and-dists))
    (define view-name (str "view" (gensym)))
    ; Return an inf representing the view.
    ; (inf
    ;   (str "view" (gensym))
      ; GENERATIVE MODEL.
      (gen [u]
        ; Sample a cluster assignment and obtain the parameters.
        (define cluster-idx
          (u (str "cluster-for-" view-name) categorical cluster-probs))
        (define params (nth cluster-params cluster-idx))
        (define cluster-idxs
          (map (gen [v] (u (str "cluster-for-" v) exactly cluster-idx))
               var-names))
        ; Sample all the variables in the view.
        (define row
          (map (gen [v] (u v apply (get vars-and-dists v) (get params v)))
               var-names))
        (zipmap var-names row))))
      ; CUSTOM INFERENCE MODEL.
      ; (gen [[t] ctx] 1))))
        ; If cluster is set, just sample.
        ; Otherwise, loop through each possible cluster, and score it.
        ; (if (constrained? t (str "cluster-for-" view-name))
        ;   [(model t) ctx 0]
        ;   (block
        ;     (define cluster-score
        ;       (gen [cluster-num]
        ;         (define logprior (log (nth cluster-probs cluster-num)))
        ;         (define params (nth cluster-params cluster-num))
        ;         (define var-score
        ;           (gen [var-name]
        ;             (if (and (targeted? t var-name) (not (intervened? t var-name)))
        ;               ((infer
        ;                  :procedure (get varset var-name),
        ;                  :inputs (get params var-name),
        ;                  :target-trace (trace-subtrace (get t :target) var-name)) 2)
        ;               0)))
        ;         (+ logprior  (apply + (map var-score (keys varset))))))
        ;     (define scores (map cluster-score (range (count cluster-probs))))
        ;     (define cluster (log-categorical scores))
        ;     (define new-intervene (trace-set-value (get t :intervene) (str "cluster-for-" view-name) cluster))
        ;     [(model (intervene-on-captured-context t new-intervene)) {} 0]))))))

(define clusters
  (gen [& args]
    [(clojure.core/take-nth 2 args)
     (clojure.core/take-nth 2 (rest args))]))

(define make-multi-mixture
  (gen [& views]
    (gen []
      (with-explicit-tracer u
        (apply merge (map (gen [view] (view u)) views))))))

(define view
  (make-view
    {"no_health_insurance" gaussian, "chlamydia" gaussian}
    (clusters
      0.46 {"no_health_insurance" [6.2 2.5] "chlamydia" [201.4 70.1]}
      0.26 {"no_health_insurance" [7.6 2.4], "chlamydia" [366.3 111.0]}
      0.14 {"no_health_insurance" [13.0 3.8], "chlamydia" [589.7 222.9]}
      0.07 {"no_health_insurance" [14.0 2.2], "chlamydia" [363.5 101.2]}
      0.03 {"no_health_insurance" [9.7 2.4], "chlamydia" [1024.7 256.7]}
      0.02 {"no_health_insurance" [5.1 1.3], "chlamydia" [644.7 125.1]}
      0.02 {"no_health_insurance" [10.6 2.3], "chlamydia" [1369.9 629.4]})))

(defn -main [& args]
  (define model (make-multi-mixture view))
  (print (model))
  (define [retval trace weight] (infer :procedure model
                                       :target-trace {"no_health_insurance" {:value 1}}))
  (print retval)
  (pprint trace)
  (print weight)
  )
  ; (print trace)
  ; (print weight))
