;; See doc/about-the-prelude.md

(ns metaprob.prelude
  (:refer-clojure :exclude [get contains? dissoc assoc empty? keys get-in map reduce replicate apply])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.trace :refer :all])
  (:require [metaprob.compound :refer :all])
  (:require [metaprob.builtin :refer :all]))


;; Eager versions of common list functions
(def map
  (gen {:tracing-with t}
    [f l]
    (doall (map-indexed (gen [i x] (t i f [x])) l))))

(def replicate
  (gen {:tracing-with t} [n f]
    (map (gen [i] (t i f [])) (range n))))

(def doall*
  (gen [s]
    (dorun (tree-seq seq? seq s)) s))

;; infer-and-score
(def primitive?
  (gen [f]
    (and (contains? f :implementation)
         (nil? (get f :model)))))

(def infer-and-score
  (gen {:tracing-with t} [& {:keys [procedure inputs observation-trace]
                                                    :or {inputs [], observation-trace {}}}]
    (t '() (make-constrained-generator procedure observation-trace) inputs)))


;
;(def infer-and-score
;  (gen {:tracing-with t, :name infer-and-score}
;    [& {:keys [procedure inputs observation-trace]
;        :or {inputs [], observation-trace {}}}]
;    (cond
;      (contains? procedure :apply?)
;      (t '() infer-and-score
;         :procedure (first inputs),
;         :inputs (second inputs),
;         :observation-trace observation-trace)
;
;      (primitive? procedure)
;      (if (trace-has-value? observation-trace)
;        ((get procedure :implementation) inputs observation-trace)
;        (let [value (t '() apply procedure inputs)]
;          [value {:value value} 0]))
;
;      (contains? procedure :implementation)
;      (t '() (get procedure :implementation) inputs observation-trace)
;
;      (get procedure :clojure-impl)
;      (let [trace (atom {})
;            score (atom 0)
;            tracer
;            (gen [addr proc & args]
;              (let [[v tr s] (t addr infer-and-score
;                                :procedure proc,
;                                :inputs args,
;                                :observation-trace (maybe-subtrace observation-trace addr))]
;                (swap! trace merge-subtrace addr tr)
;                (swap! score + s)
;                v))
;            retval (doall* (apply ((get procedure :clojure-impl) tracer) inputs))
;            final-trace (deref trace)
;            final-score (deref score)]
;        [retval final-trace final-score])
;
;      true
;      [(apply procedure inputs) {} 0])))
;
;;; Creating custom inference procedures
;(def inf
;  (gen [model implementation]
;    (assoc
;      ;; When called from Clojure:
;      (fn [& inputs]
;        (nth (implementation inputs {}) 0))
;
;      ;; Annotations:
;      :model model,
;      :implementation implementation)))