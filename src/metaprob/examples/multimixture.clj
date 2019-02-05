(ns metaprob.examples.multimixture
  (:refer-clojure :only
    [defn filter gensym])
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

; ---------
; UTILITIES
; ---------

; TODO: Migrate to math utilities module for Metaprob.
(define logsumexp
  (gen [scores]
    (define max-score (apply clojure.core/max scores))
    (define numerically-stable-scores (map (gen [x] (- x max-score)) scores))
    (define weights (map exp numerically-stable-scores))
    (define log-normalizer (+ (log (apply + weights)) max-score))
    log-normalizer))

; -------------------
; MULTI-MIXTURE MODEL
; -------------------

(define get-cluster-addr
  (gen [v]
    (str "cluster-for-" v)))

(define make-view
  (gen [vars-and-dists [cluster-probs cluster-params]]
    (define view-name (str "view" (gensym)))
    (define var-names (keys vars-and-dists))
    (define cluster-addr (get-cluster-addr view-name))
    ; GENERATIVE MODEL.
    (define sampler
      (gen []
        (with-explicit-tracer t
          ; Sample the cluster assignment.
          (define cluster-idx
            (t cluster-addr categorical cluster-probs))
          ; Set the cluster assignment for each output variable.
          ; This trick allows the INF to be CGPM-compliant, where each
          ; variable in the view has a corresponding latent variable
          ; identifying the cluster assignment. Since the cluster assignment
          ; is discrete the semantics of using exactly are well defined.
          (define cluster-idx-copies
            (map
              (gen [v] (t (get-cluster-addr v) exactly cluster-idx))
              var-names))
          ; Obtain parameters in sampled cluster.
          (define params (nth cluster-params cluster-idx))
          ; Sample the output variables.
          (map
            (gen [v] (t v apply (get vars-and-dists v) (get params v)))
            var-names))))
    ; INFERENCE MODEL.
    (define scorer
      (gen [[] ctx]
        ; Obtain the probability of trace within a fixed cluster.
        ; TODO: Consider removing the call to metaprob infer here and
        ; instead manually compute the probabilities.
        ; Advantage: will likely be faster.
        ; Disadvantage: more error prone, less elegant, less abstraction reuse.
        (define infer-apply-for-cluster
          (gen [cluster-idx]
            (define interventions (get ctx :intervene))
            (define observations
              (trace-set-value (get ctx :target)
                               cluster-addr
                               cluster-idx))
            (infer :procedure sampler
                   :target-trace observations
                   :intervention-trace interventions)))
        ; Find cluster addresses which are constrained.
        (define constrained-cluster-addresses
          (filter (gen [v] (constrained? ctx (get-cluster-addr v))) var-names))
        ; Get indexes of clusters to enumerate over.
        (define cluster-idxs
          (if
            ; Easy case: view cluster address is constrained.
            (constrained? ctx cluster-addr)
            (list (constrained-value ctx cluster-addr))
            (if
              ; Harder case: variable cluster address is constrained.
              (> (count constrained-cluster-addresses) 0)
              (block
                (assert
                  (= (count constrained-cluster-addresses) 1)
                  "Cannot specify multiple cluster constraints within a view.")
                (list
                  (constrained-value ctx
                    (get-cluster-addr (first constrained-cluster-addresses)))))
              ; No constraints, so enumerate.
              (range (count cluster-probs)))))
        (define cluster-traces (map infer-apply-for-cluster cluster-idxs))
        (define cluster-logps (map (gen [[r t w]] w) cluster-traces))
        (define [r t w] (nth cluster-traces (log-categorical cluster-logps)))
        [r t (logsumexp cluster-logps)]))
    ; RETURN THE METAPROB INF.
    (inf view-name sampler scorer)))

(define make-multi-mixture
  (gen [views]
    (gen []
      (with-explicit-tracer t
        (apply concat (map (gen [view] (t '() view)) views))))))

; ------------------------
; DOMAIN SPECIFIC LANGUAGE
; ------------------------

;; Massive hack, we cannot call `view` directly.
;; A bug in Metaprob since September makes that impossible.
;; https://github.com/probcomp/metaprob/issues/52
(define multi-mixture
  (gen [& viewspecs]
    (make-multi-mixture
      (map
        (gen [viewspec] (nth (infer :procedure make-view :inputs viewspec) 0))
        viewspecs))))

; View specification constructor.
(define view
  (gen [args] args))

; Cluster specification constructor.
(define clusters
  (gen [& args]
    [(clojure.core/take-nth 2 args)
     (clojure.core/take-nth 2 (rest args))]))
