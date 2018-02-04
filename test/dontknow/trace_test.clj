(ns dontknow.trace-test
  (:require [clojure.test :refer :all]
            [dontknow.trace :refer :all]))

; Have to use 'is' for some reason

(deftest empty-has-no-keys
  (testing "Does an empty trie have keys?"
    (is (not (has-subtrie? (new-trace) 'foo)))))

; fetch value at trie

(deftest fetch-value
  (testing "Can we get the trie's value?"
    (let [trie (new-trace 'value)]
      (is (= (value trie) 'value)))))

; store/fetch value at trie

(deftest store-fetch-value
  (testing "Can we get a value that we put in?"
    (let [trie (new-trace)]
      (set-value! trie 'value)
      (is (= (value trie) 'value)))))

; store/fetch an immediate subtrie

(deftest store-fetch-subtrie
  (testing "If you add a subtrie to a trie, is it there?"
    (let [trie (new-trace)
          sub (new-trace)]
      (set-value! sub 'value)
      (set-subtrie! trie 'key sub)
      (is (= (subtrie trie 'key) sub)))))

; store/fetch a value

(deftest store-fetch-value-at
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)]
      (set-value-at! trie '(a b c) 17)
      (is (= (value-at trie '(a b c)) 17)))))

; store/fetch a value using locative

(deftest store-fetch-value-at-2
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)
          place (subtrace trie 'a)]
      (set-value-at! place '(b) 17)
      (is (= (value-at trie '(a b)) 17)))))

(deftest narrow-1
  (testing "If you store the value of a locative, is it there?"
    (let [trie (new-trace)
          place (subtrace trie 'a)]
      (set-value! place 17)
      (is (= (value place) 17)))))

(deftest store-fetch-value-at-2a
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)
          place (subtrace-at trie '(a))]
      (set-value-at! place '(b) 17)
      (is (= (value-at trie '(a b)) 17)))))

(def suppress? true)

(deftest store-fetch-value-at-2b
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)
          place (subtrace-at trie '(a b))]
      (set-value! place 17)
      (is (= (value-at trie '(a b)) 17)))))

(deftest store-fetch-value-at-3
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)
          place (subtrace trie 'a)]
      (set-value! place 17)
      (is (= (value place) 17)))))

(deftest store-fetch-value-at-3a
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)
          place (subtrace-at trie '(a))]
      (set-value! place 17)
      (is (= (value place) 17)))))

(deftest store-fetch-value-at-4
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)
          place (subtrace-at trie '(a b))]
      (set-value! place 17)
      (is (= (value-at trie '(a b)) 17)))))

(deftest store-fetch-value-at-5
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trace)
          place (subtrace-at trie '(a b))]
      (set-value-at! place '(c d) 17)
      (is (= (value-at trie '(a b c d)) 17)))))
