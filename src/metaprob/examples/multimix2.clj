(ns metaprob.examples.multimix2
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

(define logsumexp
  (gen [scores]
    (define max-score (apply clojure.core/max scores))
    (define numerically-stable-scores (map (gen [x] (- x max-score)) scores))
    (define weights (map exp numerically-stable-scores))
    (define log-normalizer (+ (log (apply + weights)) max-score))
    log-normalizer))

(define get-cluster-addr
  (gen [v]
    (str "cluster-for-" v)))

(define clusters
  (gen [& args]
    [(clojure.core/take-nth 2 args)
     (clojure.core/take-nth 2 (rest args))]))

(define make-multi-mixture
  (gen [views]
    (gen []
      (with-explicit-tracer t
        (apply concat (map (gen [vs] (t '() vs)) views))))))

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
        (define infer-apply-for-cluster
          (gen [cluster-num]
            (define interventions (get ctx :intervene))
            (define observations
              (trace-set-value (get ctx :target)
                               cluster-addr
                               cluster-num))
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
        (print cluster-idxs)
        (define cluster-traces (map infer-apply-for-cluster cluster-idxs))
        (define cluster-logps (map (gen [[r t w]] w) cluster-traces))
        (define [r t w] (nth cluster-traces (log-categorical cluster-logps)))
        [r t (logsumexp cluster-logps)]))
    ; RETURN THE METAPROB INF.
    (inf view-name sampler scorer)))

;; Massive hack, we cannot call `view` directly.
;; A bug in Metaprob since September makes that impossible.
;; https://github.com/probcomp/metaprob/issues/52
(define multi-mixture
  (gen [& viewspecs]
    (make-multi-mixture
      (map
        (gen [viewspec] (nth (infer :procedure make-view :inputs viewspec) 0))
        viewspecs))))

(defn -main [& args]
  (define model
    (multi-mixture
      ; VIEW 1
      [{"sepal_width" gaussian
        "petal_width" gaussian
        "name" categorical
        "sepal_length" gaussian
        "petal_length" gaussian}
       (clusters
        0.325162847357 {"sepal_width" [34.179974 3.771946]
                        "petal_width" [2.440002 1.061319]
                        "name" [[0.986639 0.002751 0.002445]]
                        "sepal_length" [50.060013 3.489470]
                        "petal_length" [14.640005 1.717676]}
        0.158811538073 {"sepal_width" [29.400019 2.727630]
                        "petal_width" [14.599998 1.296148]
                        "name" [[0.007976 0.972954 0.005166]]
                        "sepal_length" [63.080017 3.877322]
                        "petal_length" [45.879978 1.986359]}
        0.152157485702 {"sepal_width" [30.666668 2.153812]
                        "petal_width" [21.291676 2.423479]
                        "name" [[0.007944 0.009516 0.968360]]
                        "sepal_length" [65.874985 2.437782]
                        "petal_length" [55.375000 2.429196]}
        0.132195328587 {"sepal_width" [26.380953 2.256758]
                        "petal_width" [11.952375 1.290116]
                        "name" [[0.011187 0.958093 0.017295]]
                        "sepal_length" [56.238064 2.327993]
                        "petal_length" [39.714233 2.762729]}
        0.0922710143592 {"sepal_width" [27.133326 2.124983]
                         "petal_width" [18.266705 2.143722]
                         "name" [[0.005149 0.009685 0.963620]]
                         "sepal_length" [59.133389 3.461565]
                         "petal_length" [49.599995 1.624789]}
        0.0656548048737 {"sepal_width" [31.363638 3.960549]
                         "petal_width" [20.909092 2.108620]
                         "name" [[0.007970 0.016508 0.947206]]
                         "sepal_length" [74.999992 2.558409]
                         "petal_length" [63.454559 3.201247]}
        0.0124223859026 {"sepal_width" [22.999990 2.160242]
                         "petal_width" [10.333331 0.471404]
                         "name" [[0.040683 0.858193 0.044297]]
                         "sepal_length" [50.000000 0.816497]
                         "petal_length" [32.666668 2.054805]}
        0.0613245951451 {"sepal_width" [30.540000 4.321466]
                         "petal_width" [11.986667 7.606126]
                         "name" [[0.333333 0.333333 0.333333]]
                         "sepal_length" [58.433333 8.253013]
                         "petal_length" [37.586667 17.585292]})]))

  (define [retval trace weight-marginal]
    (infer :procedure model
           :target-trace {"sepal_width" {:value 34}}))
  (print weight-marginal)

  (define [retval trace weight-conditional]
    (infer :procedure model
           :target-trace {"sepal_width" {:value 34}
                          "cluster-for-sepal_width" {:value 0}}
                          ))
  (print weight-conditional)
  (define [retval trace weight-conditional-impossible]
    (infer :procedure model
           :target-trace {"sepal_width" {:value 34}
                          "cluster-for-sepal_width" {:value 0}
                          "cluster-for-petal_length" {:value 2}}))
  (print weight-conditional-impossible))
