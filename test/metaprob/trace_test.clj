(ns metaprob.trace-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]))

; Predicates

(deftest trace-p
  (testing "Does an empty trace have keys?"
    (is (trace? (new-trace)))))

(deftest trie-p
  (testing "Does an empty trace have keys?"
    (is (trie? (new-trace)))))

; Have to use 'is' for some reason

(deftest empty-has-no-keys
  (testing "Does an empty trace have keys?"
    (is (not (has-subtrace? (new-trace) 'foo)))))

; fetch value at trace

(deftest fetch-value
  (testing "Can we get the trace's value?"
    (let [trace (new-trace "value")]
      (is (= (value trace) "value")))))

; store/fetch value at trace

(deftest store-fetch-value
  (testing "Can we get a value that we put in?"
    (let [trace (new-trace)]
      (set-value! trace "value")
      (is (= (value trace) "value")))))

; store/fetch an immediate subtrace

(deftest store-fetch-subtrace
  (testing "If you add a subtrace to a trace, is it there?"
    (let [trace (new-trace)
          sub (new-trace "value-1")]
      (set-value! sub "value-2")
      (set-subtrace! trace "key" sub)
      (is (= (subtrace trace "key") sub)))))

; store/fetch a value

(deftest store-fetch-value-at
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)]
      (set-value-at! trace '("a" "b" "c") 17)
      (is (= (value-at trace '("a" "b" "c")) 17)))))

; store/fetch a value using locative

(deftest store-fetch-value-at-2
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)
          place (subtrace-location trace '"a")]
      (set-value-at! place '("b") 17)
      (is (= (value-at trace '("a" "b")) 17)))))

(deftest narrow-1
  (testing "If you store the value of a locative, is it there?"
    (let [trace (new-trace)
          place (subtrace-location trace '"a")]
      (set-value! place 17)
      (is (= (value place) 17)))))

(deftest store-fetch-value-at-2a
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)
          place (subtrace-location-at trace '("a"))]
      (set-value-at! place '("b") 17)
      (is (= (value-at trace '("a" "b")) 17)))))

(deftest store-fetch-value-at-2b
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)
          place (subtrace-location-at trace '("a" "b"))]
      (set-value! place 17)
      (is (= (value-at trace '("a" "b")) 17)))))

(deftest store-fetch-value-at-3
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)
          place (subtrace-location trace '"a")]
      (set-value! place 17)
      (is (= (value place) 17)))))

(deftest store-fetch-value-at-3a
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)
          place (subtrace-location-at trace '("a"))]
      (set-value! place 17)
      (is (= (value place) 17)))))

(deftest store-fetch-value-at-4
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)
          place (subtrace-location-at trace '("a" "b"))]
      (set-value! place 17)
      (is (= (value-at trace '("a" "b")) 17)))))

(deftest store-fetch-value-at-5
  (testing "If you store a value at some address, is it there?"
    (let [trace (new-trace)
          place (subtrace-location-at trace '("a" "b"))]
      (set-value-at! place '("c" "d") 17)
      (is (has-subtrace-at? place '("c")))
      (is (has-subtrace-at? place '("c" "d")))
      (is (has-subtrace-at? trace '("a" "b" "c")))
      (is (has-subtrace-at? trace '("a" "b" "c" "d")))
      (is (= (value-at trace '("a" "b" "c" "d")) 17)))))
