(ns metaprob.examples.flip-n-coins
  (:refer-clojure :exclude [map replicate apply])
  (:require [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]))

;; Define a probabilistic model for n flips of a coin
;; with a custom address name for each coin flip

(def flip-n-coins
  (gen [n]
    (let-traced [tricky (flip 0.1)
                 p (if tricky (uniform 0 1) 0.5)]
      (map (fn [i] (at i flip p)) (range n)))))


(defn coin-flips-demo-n-flips
  [n]
  (let [[_ trace-with-n-flips _]
        (infer-and-score :procedure flip-n-coins
                         :inputs [n])]
    (infer-and-score :procedure flip-n-coins
                     :inputs [n]
                     :observation-trace trace-with-n-flips)))

;; make a partial trace that intervenes on flip-coins
;; to ensure the coin is tricky and the weight is 0.99
;; but the fourth flip comes up false

(def ensure-tricky-and-biased
  {"tricky" {:value true}
   "p" {:value 0.99}
   3 {:value false}})

(defn coin-flips-demo-biased
  [n]
  (infer-and-score :procedure flip-n-coins
                   :inputs [n]
                   :observation-trace ensure-tricky-and-biased))

