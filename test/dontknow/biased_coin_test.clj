(ns dontknow.biased-coin-test
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]
            [dontknow.builtin :refer :all]
            [dontknow.biased-coin :refer :all]))

(deftest biased-coin-flips
  (testing "So what happend?"
    (= (count (flip-coins 10)) 10)))


