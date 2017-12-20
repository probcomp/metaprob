(ns dontknow.builtin-test
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]
            [dontknow.builtin :refer :all]))

(deftest array-size-empty
  (testing "Does an empty array have size zero?"
    (= (mp*size (mp*array-from-seq [])) 0)))

(deftest array-size-nonempty
  (testing "Does a nonempty array have size greater than zero?"
    (= (mp*size (mp*array-from-seq [3 7])) 2)))

(deftest array-inversion-empty
  (testing "Can we convert an empty array to a seq and back?"
    (let [victim [19 23 44 7]]
      (= (seq-from-mp*array (mp*array-from-seq victim)))
         victim)))

