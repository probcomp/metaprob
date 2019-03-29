(ns metaprob.syntax-test
  (:refer-clojure :exclude [apply contains? dissoc assoc empty? keys get-in map replicate reduce])
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer :all]))

(deftest gen-1
  (testing "Smoke test for gen macro"
    (is (= ((gen [x] x) 1) 1))))

(deftest gen-2
  (testing "Procedures are (no longer) traces"
    (is (not (trace? (gen [x] x))))))

(deftest gen-3
  (testing "are procedures named?"
    (is (= (get (meta (gen {:name foo} [x] x)) :name) 'foo))))