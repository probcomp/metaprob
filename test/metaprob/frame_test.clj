(ns metaprob.frame-test
  (:require [clojure.test :refer :all]
            [metaprob.syntax :as syntax]
            [metaprob.builtin :as builtin]
            [metaprob.frame :refer :all]))

(def top (builtin/make-top-level-env 'metaprob.frame))

(deftest frame-1
  (testing "frame smoke test"
    (let [f (make-env top)]
      (env-bind! f "foo" 17)
      (is (= (env-lookup f "foo") 17))
      (is (= (env-lookup f "sub") builtin/sub)))))

(deftest frame-2
  (testing "match-bind! smoke test"
    (let [f (make-env top)
          pat (syntax/from-clojure-pattern '[a b])]
      (match-bind! pat (list 1 2) f)
      (is (= (env-lookup f "a") 1))
      (is (= (env-lookup f "b") 2)))))

