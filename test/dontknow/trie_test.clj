(ns dontknow.trie-test
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]))

; Have to use 'is' for some reason

(deftest empty-has-no-keys
  (testing "Does an empty trie have keys?"
    (is (not (has-subtrie? (new-trie) 'foo)))))

; fetch value at trie

(deftest fetch-value
  (testing "Can we get the trie's value?"
    (is (let [trie (new-trie 'value)]
          (= (value trie) 'value)))))

; store/fetch value at trie

(deftest store-fetch-value
  (testing "Can we get a value that we put in?"
    (let [trie (new-trie)]
      (set-value! trie 'value)
      (= (value trie) 'value))))

; store/fetch an immediate subtrie

(deftest store-fetch-subtrie
  (testing "If you add a subtrie to a trie, is it there?"
    (let [trie (new-trie)
          sub (new-trie)]
      (set-value! sub 'value)
      (set-subtrie! trie 'key sub)
      (= (subtrie trie 'key) sub))))

; store/fetch a value

(deftest store-fetch-value-at
  (testing "If you store a value at some address, is it there?"
    (let [trie (new-trie)]
      (set-value-at! trie '[a b c] 17)
      (= (value-at trie '[a b c]) 17))))
