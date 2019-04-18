(ns metaprob.cgpm-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc source]]
            [clojure.test :refer :all]
            [clojure.string :refer [index-of]]
            [metaprob.generative-functions :refer :all]
            [metaprob.code-handlers :refer :all]
            [metaprob.expander :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.autotrace :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.inference :refer :all]
            [metaprob.distributions :refer :all]
    [metaprob.examples.cgpm :refer :all]
    [metaprob.examples.multimixture-dsl :refer :all]))


; ---------
; UTILITIES
; ---------

(def abs (fn [n] (max n (- n))))
(def relerr (fn [a b] (abs (- a b))))

(defn make-identity-output-addr-map
  [output-addrs-types]
    (let [output-addrs (keys output-addrs-types)
          trace-addrs  (map clojure.core/name output-addrs)]
    (clojure.core/zipmap output-addrs trace-addrs)))

(deftest test-make-identity-output-addr-map
  (is (= (make-identity-output-addr-map {:d 1 :f 1})
         {:d "d" :f "f"})))

; --------------------
; TESTS FOR DUMMY CGPM
; --------------------

(def generate-dummy-row
  (gen [y]
      (let [x0 (at "x0" uniform-discrete [[1 2 3 4]])
            x1 (at "x1" uniform [9 199])
            x2 (at "x2" gaussian [0 10])
            x3 (at "x3" uniform-discrete [["foo" "bar" "baz"]])]
      [x0 x1 x2 x3])))

(def dummy-cgpm
    (let [
          inputs-addrs-types  {:y real-type}
          outputs-addrs-types {:x0 (make-nominal-type #{1 2 3 4})
                               :x1 (make-ranged-real-type 9 199)
                               :x2 real-type
                               :x3 (make-nominal-type #{"foo" "bar" "baz"})}
           output-addr-map     (make-identity-output-addr-map outputs-addrs-types)
           input-addr-map      {:y 0}]
      (make-cgpm generate-dummy-row
                 outputs-addrs-types
                 inputs-addrs-types
                 output-addr-map
                 input-addr-map)))

; TODO: Write tests which capture assertion fails for initialize.
; 1. output-addrs and and input-addrs overlap.
; 2. output-addr-map is missing keys.
; 3. input-addr-map is missing keys.
; 4. output-addr-map has duplicate values.
; 5. input-addr-map map to non-integers.
; 6. input-addr-map map to non-contiguous integers.

; TODO: Write tests which capture assertion fails for simulate/logpdf errors:
; 1. Unknown variable in target.
; 2. Unknown variable in constraint.
; 3. Unknown variable in input.
; 4. Overlapping target and constraint in logpdf.
; 5. Provided values disagree with statistical data types.

; TODO: Write tests which capture assertion fails for KL divergence errors:
; 1. Different base measures of target-addrs-0 and target-addrs-1.

(deftest dummy-row-logpdf
  (is (< (cgpm-logpdf dummy-cgpm {:x0 2} {} {:y 100}) 0))
  (is (< (cgpm-logpdf dummy-cgpm {:x1 120} {:x0 2} {:y 100})))
  (is (< (cgpm-logpdf dummy-cgpm {:x0 2 :x1 120} {} {:y 100}))))

