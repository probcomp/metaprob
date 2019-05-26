(ns metaprob.autodiff-test
  (:refer-clojure :exclude [apply map replicate])
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.autodiff :as autodiff :refer [diff lift-real->real]]
            [metaprob.prelude :as prelude :refer [apply map replicate]]
            [metaprob.distributions :as dist]))

(deftest deriv-of-identity
  (testing "derivative of the identity function"
    (is (== ((diff (lift-real->real (fn [x] x)
                                    (fn [x] 1)))
             7.0)
            1.0))))
