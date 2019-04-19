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
       (let [x0 (at "x0" uniform-discrete [1 2 3 4])
             x1 (at "x1" uniform 9 199)
             x2 (at "x2" gaussian 0 10)
             x3 (at "x3" uniform-discrete ["foo" "bar" "baz"])]
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
  (is (< (cgpm-logpdf dummy-cgpm {:x1 120} {:x0 2} {:y 100}))) ;; XXX -- what's that
  (is (< (cgpm-logpdf dummy-cgpm {:x0 2 :x1 120} {} {:y 100})))) ;; XXX -- what's that

(deftest dummy-row-simulate
  (testing "dummy-row-simulate"
    (let
      [sample-1 (cgpm-simulate dummy-cgpm [:x0 :x1 :x2] {} {:y 100} 10)
       sample-2 (cgpm-simulate
                   dummy-cgpm
                   [:x0 :x1 :x2 :x3]
                   {:x3 "foo"}
                   {:y 100} 20)]
      (is (= (count sample-1) 10))
      (is (= (count sample-2) 20))
      (is (= (get (nth sample-2 0) :x3) "foo")))))

(deftest dummy-row-mutual-information-zero
  (testing "dummy-row-mutual-information-zero"
    (is (< (cgpm-mutual-information dummy-cgpm [:x0] [:x1] [] {:x3 "foo"}
                                               {:y 100} 10 1)
            1E-10))))

(deftest dummy-row-kl-divergence-zero
  (testing "dummy-row-kl-divergence-zero"
  (is (< (cgpm-kl-divergence dummy-cgpm [:x0] [:x0] {} {:x3 "foo"} {:y 100} 10)
         1E-10))))

(deftest dummy-row-kl-divergence-non-zero
  (testing "dummy-row-kl-divergence-non-zero"
  (is (> (cgpm-kl-divergence dummy-cgpm [:x1] [:x2] {} {:x3 "foo"} {:y 100} 10)
         1))))

; --------------------------
; TEST FOR MULTIMIXTURE CGPM
; --------------------------

(def generate-crosscat-row
  (multi-mixture
    ;; XXX: here, there is a change I don't fully understand -- Feras' multi-mixture takes a list of views,
    ;; Alex's doesn't.
    (view
      {"sepal_width" gaussian
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
                         "petal_length" [37.586667 17.585292]}))))

(def crosscat-cgpm
  (let [outputs-addrs-types {; Variables in the table.
                              :sepal_length real-type
                              :sepal_width real-type
                              :petal_length real-type
                              :petal_width real-type
                              :name integer-type
                              ; Exposed latent variables.
                              :cluster-for-sepal_length integer-type
                              :cluster-for-sepal_width integer-type
                              :cluster-for-petal_length integer-type
                              :cluster-for-petal_width integer-type
                              :cluster-for-name integer-type}
        output-addr-map (make-identity-output-addr-map outputs-addrs-types)
        inputs-addrs-types {}
        input-addr-map {}]
  (make-cgpm generate-crosscat-row
             outputs-addrs-types
             inputs-addrs-types
             output-addr-map
             input-addr-map)))

; TODO: Provide coverage for the following cases:
; 1. A MultiMixture CGPM with more than one view.

; TODO: Write tests which capture assertion fails for simulate/logpdf errors:
; 1. cluster-for-[varname] in same view contradict each other.

(deftest crosscat-row-logpdf-agree-data
  (testing "crosscat-row-logpdf-agree-data"
    (let [lp-multimix (nth
                        (infer-and-score
                          :procedure generate-crosscat-row
                          :observation-trace {"sepal_width" {:value 34}})
                       2)
          lp-cgpm (cgpm-logpdf crosscat-cgpm {:sepal_width 34} {} {})]
    (is (< (relerr lp-cgpm lp-multimix) 1E-4)))))

(deftest crosscat-row-logpdf-agree-cluster
  (testing "crosscat-row-logpdf-agree-cluster"
    (let [lp-multimix (nth
                        (infer-and-score
                          :procedure generate-crosscat-row
                          :observation-trace {"cluster-for-sepal_length" {:value 0}})
                        2)
          lp-cgpm (cgpm-logpdf crosscat-cgpm
                    {:cluster-for-sepal_length 0} {} {})]
    (is (< (relerr lp-cgpm lp-multimix) 1E-4)))))

(deftest crosscat-row-logpdf-agree-joint
  (testing "crosscat-row-logpdf-agree-joint"
    (let [lp-multimix (nth
                        (infer-and-score
                          :procedure generate-crosscat-row
                          :observation-trace {"sepal_width" {:value 34}
                                         "cluster-for-sepal_length" {:value 0}})
                        2)
          lp-cgpm (cgpm-logpdf
                    crosscat-cgpm
                    {:sepal_width 34 :cluster-for-sepal_length 0}
                      {} {})]
    (is (< (relerr lp-cgpm lp-multimix) 1E-4)))))

(deftest crosscat-row-logpdf-conditional
  (testing "crosscat-row-logpdf-conditional"
    (let [lp-zx (cgpm-logpdf
                  crosscat-cgpm
                  {:sepal_width 34 :cluster-for-sepal_length 0}
                  {} {})
          lp-z (cgpm-logpdf
            crosscat-cgpm
              {:cluster-for-sepal_length 0}
              {} {})
          lp-x-given-z (cgpm-logpdf
                          crosscat-cgpm
                          {:sepal_width 34}
                          {:cluster-for-sepal_length 0}
                          {})
          lp-x (cgpm-logpdf
                 crosscat-cgpm
                 {:sepal_width 34}
                 {} {})
          lp-z-given-x (cgpm-logpdf
                         crosscat-cgpm
                         {:cluster-for-sepal_length 0}
                         {:sepal_width 34}
                         {})]
    (is (< (relerr lp-x-given-z (- lp-zx lp-z)) 1E-4))
    (is (< (relerr lp-z-given-x (- lp-zx lp-x)) 1E-4)))))

; TODO: Only run test under --slow option.
(deftest crosscat-row-mi-nonzero
  (testing "crosscat-row-mi-nonzero"
    (let [mi (cgpm-mutual-information
               crosscat-cgpm
               [:sepal_length]
               [:sepal_width]
               {} {} {} 100 1)]
    (is (> mi 0.05)))))

; TODO: Only run test under --slow option.
(deftest crosscat-row-mi-conditional-indep-fixed-z
  (testing "crosscat-row-mi-conditional-indep-fixed-z"
    (let [mi (cgpm-mutual-information
               crosscat-cgpm
               [:sepal_length]
               [:sepal_width]
               {}
               {:cluster-for-sepal_length 0}
               {} 50 10)]
    (is mi (< 1E-5)))))

; TODO: Only run test under --slow option.
(deftest crosscat-row-mi-conditional-indep-marginalize-z
  (testing "crosscat-row-mi-conditional-indep-marginalize-z"
    (let [mi (cgpm-mutual-information
               crosscat-cgpm
               [:sepal_length]
               [:sepal_width]
               [:cluster-for-sepal_length]
               {}
               {} 50 1)]
      (is mi (< 1E-5)))))

(deftest crosscat-cgpm-simulate
  (testing "crosscat-cgpm-simulate"
    (let [num-samples 10
          samples     (cgpm-simulate
                       crosscat-cgpm
                       [:cluster-for-sepal_length]
                       {}
                       {}
                       num-samples)]
      (is (= (count samples)
             num-samples)))))


;; crosscat latent variables
(def latent-variables  #{:cluster-for-sepal_length})
(def row-1 {:sepal_width  34.179974
            :petal_width  2.440002
            :name         0
            :sepal_length 50.060013
            :petal_length 14.640005})
(def row-2 {:sepal_width  200
            :petal_width  200
            :name         2
            :sepal_length 200
            :petal_length  200})


;; row wise-similarity. For seach-by-example, make row-1 a hypothetical row and
;; then loop over each row the data table (and make each row-2).
(deftest row-wise-similarity
  (testing "row-wise-similarity"
  (let [symmetrized-kl (+ (cgpm-kl-divergence
                            crosscat-cgpm
                            latent-variables
                            latent-variables
                            row-1
                            row-2
                            {}
                            10)
                          (cgpm-kl-divergence
                            crosscat-cgpm
                            latent-variables
                            latent-variables
                            row-2
                            row-1
                            {}
                            10))]
  (is (> symmetrized-kl 0)))))
