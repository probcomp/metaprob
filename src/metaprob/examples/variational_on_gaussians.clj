(ns metaprob.examples.variational-on-gaussians
  (:require [metaprob.inference :as infer]
            [clojure.pprint :refer [pprint]]
            [metaprob.distributions :as dist]
            [metaprob.prelude :as mp]
            [metaprob.trace :as trace]
            [metaprob.generative-functions :refer [at gen let-traced]]
            [metaprob.autodiff :as ad]))

(def beta-binomial
  (gen [n]
    (let-traced [p1 (dist/beta 1 1)
                 p2 (dist/beta 1 1)]
      (doall (map #(at % dist/flip (if (at `("which-p" ~%) dist/flip 0.5) p1 p2)) (range n))))))

(defn make-observation-trace [l]
  (first (reduce (fn [[tr i] x] [(trace/trace-set-value tr i x) (inc i)]) [{} 0] l)))

(defn initial-trace [l]
  (trace/trace-clear-subtrace
    ((mp/infer-and-score :procedure beta-binomial
                         :inputs [(count l)]
                         :observation-trace (make-observation-trace l)) 1)
    "which-p"))


(defn map-optimize-demo []
  (pprint
    (let [tr
          (time
            (last
              (take 200
                (iterate
                  (infer/map-optimize-step
                    :model beta-binomial
                    :inputs [200]
                    :step-size 0.0001
                    :addresses '("p1" "p2"))
                  (initial-trace (vec (concat (repeat 100 true) (repeat 100 false))))))))]
  [(trace/trace-value tr "p1") (trace/trace-value tr "p2")])))


; Model
(def normal-normal
  (gen []
    (let-traced [x (dist/gaussian 0 1)
                 y (dist/gaussian x 1)]
      y)))

; Guide
(defn normal-normal-predicter
  [[a b sigma2]]
  (gen [obs]
    (let [y (trace/trace-value obs "y")]
      (at "x" dist/gaussian
          (ad/+ b (ad/* a y))
          (ad/sqrt sigma2)))))

;; Amortized inference: minimize KL(model || guide)
(defn train-normal-normal-predicter-amortized []
  (let [final-params
        (time
          (last
            (take 5000
                  (iterate
                    (fn [params]
                      (infer/train-amortized-inference-program
                        :model normal-normal
                        :inference-program normal-normal-predicter
                        :observation-addresses ["y"]
                        :step-size 0.01
                        :batch-size 10
                        :current-params params))
                    [1 1 1]))))]
    (println final-params)
    (normal-normal-predicter final-params)))

;; Variational inference: minimize KL(guide || model)
(defn train-normal-normal-predicter-variational []
  (let [final-params
        (time
          (last
            (take 5000
                  (iterate
                    (fn [params]
                      (infer/reparam-variational-inference
                        :model normal-normal
                        :guide normal-normal-predicter
                        :observation-addresses ["y"]
                        :step-size 0.01
                        :current-params params))
                    [1 1 1]))))]
    (println final-params)
    (normal-normal-predicter final-params)))

;; Score Variational inference: minimize KL(guide || model)
(defn train-normal-normal-predicter-variational-score []
  (let [final-params
        (time
          (last
            (take 10000
                  (iterate
                    (fn [params]
                      (infer/score-func-variational-inference
                        :model normal-normal
                        :guide normal-normal-predicter
                        :observation-addresses ["y"]
                        :step-size 0.0005
                        :current-params params))
                    [1 1 1]))))]
    (println final-params)
    (normal-normal-predicter final-params)))


(defn normal-normal-demo []
  (train-normal-normal-predicter-amortized)
  (train-normal-normal-predicter-variational)
  (train-normal-normal-predicter-variational-score))


(def mixture-model
  (mp/make-primitive
    (fn [] (if (dist/flip 0.5) (dist/gaussian -5 1) (dist/gaussian 5 1)))
    (fn [x []] (ad/log
                 (ad/+ (ad/* 0.5 (ad/exp (dist/score-gaussian x [-5 1])))
                       (ad/* 0.5 (ad/exp (dist/score-gaussian x [5 1]))))))))

(defn mixture-predicter
  [[mu sigma]]
  (gen [obs]
    (at '() dist/gaussian mu sigma)))

;; Amortized inference: minimize KL(model || guide)
(defn train-mixture-predicter-amortized []
  (let [final-params
        (time
          (last
            (take 6000
                  (iterate
                    (fn [params]
                      (infer/train-amortized-inference-program
                        :model mixture-model
                        :inference-program mixture-predicter
                        :observation-addresses []
                        :step-size 0.01
                        :batch-size 10
                        :current-params params))
                    [2 1]))))]
    (println final-params)

    (mixture-predicter final-params)))

;; Variational inference: minimize KL(guide || model)
(defn train-mixture-predicter-variational []
  (let [final-params
        (time
          (last
            (take 30000
                  (iterate
                    (fn [params]
                      (infer/reparam-variational-inference
                        :model mixture-model
                        :guide mixture-predicter
                        :observation-addresses []
                        :step-size 0.05
                        :current-params params))
                    [0 1]))))]
    (println final-params)
    (mixture-predicter final-params)))


;; Variational inference: minimize KL(guide || model)
(defn train-mixture-predicter-variational-score []
  (let [final-params
        (time
          (last
            (take 30000
                  (iterate
                    (fn [params]
                      (infer/score-func-variational-inference
                        :model mixture-model
                        :guide mixture-predicter
                        :observation-addresses []
                        :step-size 0.05
                        :current-params params))
                    [0 1]))))]
    (println final-params)
    (mixture-predicter final-params)))


(defn mixture-demo []
  (train-mixture-predicter-amortized)
  (train-mixture-predicter-variational)
  (train-mixture-predicter-variational-score))


(defn -main []
  (map-optimize-demo)
  (normal-normal-demo)
  (mixture-demo))

;; => {"p" {:value 0.3594875896}, "y" {:value false}}