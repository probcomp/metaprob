(ns metaprob.examples.multimixture-dsl
  (:refer-clojure :exclude [map replicate apply])
  (:require [metaprob.trace :refer :all]
            [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [clojure.pprint :refer [pprint]]
            [metaprob.inference :refer :all]))


; -------------------
; MULTI-MIXTURE MODEL
; -------------------

(defn get-cluster-addr
  [v]
  (str "cluster-for-" v))


(defn make-view
  [[vars-and-dists [cluster-probs cluster-params]]]
  (let [view-name (str "view" (gensym))
        var-names (keys vars-and-dists)
        cluster-addr (get-cluster-addr view-name)

        ;; Generative model
        sampler
        (gen []
          (let [cluster-idx (at cluster-addr categorical cluster-probs)
                params  (nth cluster-params cluster-idx)]
            (map (fn [v] (apply-at v (get vars-and-dists v) (get params v))) var-names)))]
     (with-custom-proposal-attached
       sampler
       (fn [observations]
         (gen []
           (let [score-cluster
                 (fn [idx]
                   (let [new-obs (trace-set-value observations cluster-addr idx)
                         ;; Score should not depend on any of the stochastic
                         ;; choices made by infer-and-score, so we leave this
                         ;; untraced.
                         [_ _ s]
                         (infer-and-score
                           :procedure sampler
                           :observation-trace new-obs)]
                     s))

                 cluster-scores
                 (map score-cluster (range (count cluster-probs)))

                 chosen-cluster
                 (at cluster-addr log-categorical cluster-scores)]

             ;; Fill in the rest of the choices
             (at '() infer-and-score
                 :procedure sampler
                 :observation-trace
                 (trace-set-value observations cluster-addr chosen-cluster)))))

       ;; Only use the custom proposal when we don't already know the cluster ID
       (fn [tr] (not (trace-has-value? tr cluster-addr))))))

(defn make-multi-mixture
  [views]
  (gen []
    (apply concat (map (fn [view] (at '() view)) views))))

; ------------------------
; DOMAIN SPECIFIC LANGUAGE
; ------------------------

(defn multi-mixture
  [& viewspecs]
  (make-multi-mixture (map make-view viewspecs)))

; View specification constructor.
(defn view [vars [probs params]] [vars [probs params]])

; Cluster specification constructor.
(defn clusters
  [& args]
  [(take-nth 2 args)
   (take-nth 2 (rest args))])


;; Create an example model using the DSL
(def example-model
  (multi-mixture
    (view
      {"x" gaussian, "y" gaussian}
      (clusters
        0.3 {"x" [0 1], "y" [1 5]}
        0.2 {"x" [0 10], "y" [10 15]}
        0.5 {"x" [4 2], "y" [1 6]}))
    (view
      {"a" categorical "b" categorical}
      (clusters
        0.8 {"a" [[0.1 0.4 0.5]] "b" [[0.1 0.9]]}
        0.2 {"a" [[0.9 0.1 0.0]] "b" [[0.5 0.5]]}))))

(defn -main
  []
  (pprint (example-model))
  (pprint (infer-and-score :procedure example-model
                           :observation-trace {"a" {:value 0}
                                               "b" {:value 0}})))