(ns metaprob.examples.test_cgpm_multimixture
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
    [metaprob.examples.gaussian :refer :all]
    [metaprob.examples.cgpm :refer :all]
    [metaprob.examples.multimixture :refer :all]))

; The dummy Metaprob gen that will be converted into a CGPM.
(define generate-dummy-row
  (gen [y]
    (with-explicit-tracer t
      (define x0 (t "x0" uniform-sample [1 2 3 4]))
      (define x1 (t "x1" uniform 9 199))
      (define x2 (t "x2" gaussian 0 10))
      (define x3 (t "x3" labeled-categorical ["foo" "bar" "baz"]
                                             [0.25 0.5 0.25]))
      [x0 x1 x2 x3])))

; The CrossCat Metaprob gen that will be converted into a CGPM.
(define generate-crosscat-row
  (multi-mixture
    (view
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
                         "petal_length" [37.586667 17.585292]})])))

(defn -main [& args]

  ; DUMMY ROW TEST.
  (define outputs-addrs-types
    {:x0 (make-nominal-type #{1 2 3 4})
     :x1 (make-ranged-real-type 9 199)
     :x2 real-type
     :x3 (make-nominal-type #{"foo" "bar" "baz"})})
  (define inputs-addrs-types {:y real-type})
  (define output-addr-map {:x0 "x0", :x1 "x1", :x2 "x2", :x3 "x3"})
  (define input-addr-map {:y 0})
  (define cgpm
    (make-cgpm generate-dummy-row
               outputs-addrs-types
               inputs-addrs-types
               output-addr-map
               input-addr-map))
  ; Example queries on simulate and logpdf.
  (print (cgpm-logpdf cgpm {:x0 2} {} {:y 100}))
  (print (cgpm-logpdf cgpm {:x1 120} {:x0 2} {:y 100}))
  (print (cgpm-logpdf cgpm {:x0 2 :x1 120} {} {:y 100}))
  (print (cgpm-simulate cgpm [:x0 :x1 :x2] {} {:y 100} 10))
  (print (cgpm-simulate cgpm [:x0 :x1 :x2] {:x3 "foo"} {:y 100} 10))
  ; Example queries on mutual information and KL divergence.
  (clojure.core/assert
    (< (cgpm-mutual-information cgpm [:x0] [:x1] [] {:x3 "foo"}
                                     {:y 100} 1 1))
    1E-10)
  (clojure.core/assert
    (< (cgpm-kl-divergence cgpm [:x0] [:x0] [] {:x3 "foo"} {:y 100} 10)
       1E-10))
  (print (cgpm-kl-divergence cgpm [:x1] [:x2] [] {} {:y 100} 1000))

  ; CROSSCAT TEST.

  (define [retval trace weight-marginal]
    (infer :procedure generate-crosscat-row
           :target-trace {"sepal_width" {:value 34}}))
  (print weight-marginal)

  (define [retval trace weight-conditional]
    (infer :procedure generate-crosscat-row
           :target-trace {"sepal_width" {:value 34}
                          "cluster-for-sepal_width" {:value 0}}
                          ))
  (print weight-conditional))

  ; (define [retval trace weight-conditional-impossible]
  ;   (infer :procedure generate-crosscat-row
  ;          :target-trace {"sepal_width" {:value 34}
  ;                         "cluster-for-sepal_width" {:value 0}
  ;                         "cluster-for-petal_length" {:value 2}}))
  ; (print weight-conditional-impossible))
