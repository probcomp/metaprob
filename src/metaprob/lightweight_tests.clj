(ns metaprob.lightweight-tests
  (:refer-clojure :exclude [apply get contains? dissoc assoc empty? keys get-in map replicate reduce])
  (:require [clojure.pprint :as pprint]
            [metaprob.builtin :refer :all]
            [metaprob.compound :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.autotrace :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.syntax :refer :all]))

;(def g (gen {:tracing-with t} [] (map (fn [i] (t i flip [0.5])) (range 2))))
;
;(def g
;  (gen {:transform "autotrace"} []
;    (do (flip 0.2) (replicate 2 (gen [] (flip 0.5))))))

(def g
  (gen {:transform "autotrace"} []
    (let [p (beta 1 1)
          f (flip p)]
      (if f
        (replicate 2 #(flip p))
        (replicate 3 #(flip p))))))

;(def g
;  (gen {:transform "autotrace"} []
;    (log-categorical [1 1 1]) (flip 0.2)))

(def f (gen {:tracing-with t} [] (t '("hello") log-categorical [[1 1 1]]) (t '("goodbye") flip [0.2])))

(defn -main []
  (pprint/pprint (g))
  (pprint/pprint (infer-and-score :procedure g))
  (pprint/pprint (meta g))
  (pprint/pprint (infer-and-score :procedure f :observation-trace {"hello" {:value 1}}))
  (pprint/pprint (infer-and-score :procedure infer-and-score :inputs [:procedure f :observation-trace {"hello" {:value 1}}]))
  (pprint/pprint (infer-and-score :procedure infer-and-score
                                  :inputs [:procedure f :observation-trace {"hello" {:value 1}}]
                                  :observation-trace {"goodbye" {:value false}}))
  ;(let
  ;  [f (gen (:tracing-with t) [x] (t '("hi") exactly (+ 2 x)))]
  ;  (pprint/pprint (f 1))
  ;  (pprint/pprint (infer-and-score f [1] {})))
  ;; (pprint/pprint (mp-expand test-case))
  ;; (print (infer-and-score (gen [x] (2)) [1] {}))
  ;(print (get (gen [x] [t]) :generative-source))
  ;(print ((gen [x] [(t 1 + 2 3)]) 1))
  (newline)
  (flush))
