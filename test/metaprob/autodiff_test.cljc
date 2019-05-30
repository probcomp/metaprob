(ns metaprob.autodiff-test
  (:refer-clojure :exclude [apply map replicate])
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.autodiff :as autodiff :refer [diff lift-real->real recursive-unnest-value]]
            [metaprob.prelude :as prelude :refer [apply map replicate]]
            [metaprob.distributions :as dist]))


(defn absdiff [x y]
  (Math/abs (- x y)))

(defn finite-difference [f x eps]
  (/ (- (f (+ x eps))
        (f (- x eps)))
     (* 2.0 eps)))


(deftest deriv-of-identity
  (testing "derivative of the identity function"
    (is (== ((diff (lift-real->real (fn [x] x)
                                    (fn [x] 1)))
             7.0)
            1.0))))

(deftest compare-to-finite-differences
  (testing "compare autodiff to finite-differences approximation"
    (def compare-single
      "Compares the value of the derivative of `f` as computed by autodiff vs.
      finite differences, at the point `x`."
      (fn [f f-name x epsilon atol]
        (is (< (absdiff ((diff f) x)
                        (finite-difference f x epsilon))
               atol)
            (str "Autodiff disagrees with finite differences for " f-name))))

    (let [f-vars [
                  #'autodiff/sqrt
                  #'autodiff/exp
                  #'autodiff/log
                  #'autodiff/log1p
                  #'autodiff/sin
                  #'autodiff/cos
                  #'autodiff/tan
                  #'autodiff/asin
                  #'autodiff/acos
                  #'autodiff/atan
                  #'autodiff/sinh
                  #'autodiff/cosh
                  #'autodiff/tanh
                  ]
          n (count f-vars)
          xs (repeat n 0.6)
          epsilons (repeat n 1e-4)
          atols (repeat n 1e-5)]
      (doall (map compare-single
                  (map var-get f-vars)
                  (map (comp :name meta) f-vars)
                  xs
                  epsilons
                  atols)))))
