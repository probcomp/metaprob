(ns dontknow.builtin-test
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]
            [dontknow.builtin :refer :all]))

(deftest program-1
  (testing "Smoke test for program macro"
    (is (= ((program [x] x) 1) 1))))

(deftest block-1
  (testing "Smoke test 1 for block macro"
    (is (= (block 1) 1))))

(deftest block-2
  (testing "Smoke test 2 for block macro"
    (is (= (block 1 2) 2))))

(deftest block-let-1
  (testing "Smoke test for block macro + define"
    (is (= (block (define foo 3) foo) 3))))

(deftest block-let-2
  (testing "Smoke test 2 for block macro + define"
    (is (= (block (define foo 3) (define bar (+ foo 1)) bar) 4))))

(deftest block-letfn-1
  (testing "Smoke test for block macro"
    (is (= (block (define foo (program [x] x)) (foo 7)) 7))))

(deftest block-mixed-1
  (testing "Smoke test for block macro"
    (is (= (block (define foo (program [x] x)) (define bar 7) (foo bar)) 7))))

(deftest array-size-empty
  (testing "Does an empty array have size zero?"
    (is (= (mp*size (mp*array-from-seq [])) 0))))

(deftest array-size-nonempty
  (testing "Does a nonempty array have size greater than zero?"
    (is (= (mp*size (mp*array-from-seq [3 7])) 2))))

(deftest array-inversion-empty
  (testing "Can we convert an empty array to a seq and back?"
    (let [victim [19 23 44 7]]
      (is (= (seq-from-mp*array (mp*array-from-seq victim))
             victim)))))

(deftest reification-1
  (testing "Does a program appear to be a trie?"
    (is (= (trace_get (program [] 7)) "prob prog"))))


