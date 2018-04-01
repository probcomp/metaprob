(ns metaprob.basic-trace-test
  (:require [clojure.test :refer :all]
            [metaprob.basic-trace :refer :all]))

; Predicates

(deftest trace-p
  (testing "Does an empty trace have keys?"
    (is (basic-trace? (mutable-trace)))))

(deftest trie-p
  (testing "Does an empty trace have keys?"
    (is (trie? (mutable-trace)))))

; Have to use 'is' for some reason

(deftest empty-has-no-keys
  (testing "Does an empty trace have keys?"
    (is (not (has-subtrace? (mutable-trace) 'foo)))))

; fetch value at trace

(deftest fetch-value
  (testing "Can we get the trace's value?"
    (let [trace (mutable-trace "value")]
      (is (= (value trace) "value")))))

; store/fetch value at trace

(deftest store-fetch-value
  (testing "Can we get a value that we put in?"
    (let [trace (mutable-trace)]
      (set-value! trace "value")
      (is (= (value trace) "value")))))

; store/fetch an immediate subtrace

(deftest store-fetch-subtrace
  (testing "If you add a subtrace to a trace, is it there?"
    (let [trace (mutable-trace)
          sub (mutable-trace "value-1")]
      (set-value! sub "value-2")
      (set-subtrace! trace "key" sub)
      (is (= (subtrace trace "key") sub)))))
