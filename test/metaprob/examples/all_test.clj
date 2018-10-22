(ns metaprob.examples.all-test
  (:require [clojure.test :refer :all]))

(deftest test-require
  (testing "can the namespace be required"
    (is (any? (require 'metaprob.examples.all)))))
