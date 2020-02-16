(ns metaprob.crp-test
  (:refer-clojure :exclude [apply map replicate])
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]
            [metaprob.prelude :as prelude :refer [apply map replicate]]
            [metaprob.generative-functions :as gen :refer [gen]]
            [metaprob.distributions :as dist]))

(deftest crp-1
  (testing "CRP smoke tests"
    0))
