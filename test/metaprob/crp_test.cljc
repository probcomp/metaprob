(ns metaprob.crp-test
  ;; Header copied from distributions_test.cljc
  (:refer-clojure :exclude [apply map replicate])
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]
            [metaprob.prelude :as prelude :refer [apply map replicate]]
            [metaprob.generative-functions :as gen :refer [gen]]
            [metaprob.crp :as crp]))

(deftest crp-smoke-1
  (testing "CRP smoke tests"
    (let [alpha 2.0
          iterations 100
          state (nth (iterate (fn [state]
                                (let [table (crp/sample state alpha)]
                                  (crp/incorporate state table)))
                              [])
                     iterations)]
      ;; If you're curious what the states look like:
      ;; (binding [*out* *err*] (println state))
      (is (= (clojure.core/reduce + state) iterations))
      (is (every? (fn [count] (> count 0)) state))
      (is (< (count state) iterations)))))
