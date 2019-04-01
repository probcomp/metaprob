(ns metaprob.syntax-test
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]
            [metaprob.generative-functions :refer [gen]]))

(deftest gen-1
  (testing "Smoke test for gen macro"
    (is (= ((gen [x] x) 1) 1))))

(deftest gen-2
  (testing "Procedures are (no longer) traces"
    (is (not (trace/trace? (gen [x] x))))))

(deftest gen-3
  (testing "are procedures named?"
    (is (= (get (meta (gen {:name foo} [x] x)) :name) 'foo))))
