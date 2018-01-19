(ns dontknow.library-test
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]
            [dontknow.library :refer :all]))

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

(deftest reification-1
  (testing "Does a program appear to be a trie?"
    (is (= (trace_get (program [] 7)) "prob prog"))))

(deftest map-1
  (testing "Map over a clojure list"
    (is (= (mp-map (fn [x] (+ x 1))
                   (list 6 7 8))
           (list 7 8 9)))))

(deftest map-2
  (testing "Map over a metaprob list"
    (is (= (mp-first
            (mp-rest
             (mp-map (fn [x] (+ x 1))
                     (pair 6 (pair 7 (pair 8 (mk_nil)))))))
           8))))

(deftest map-3
  (testing "Map over a metaprob array/tuple"
    (is (= (value-at (mp-map (fn [x] (+ x 1))
                             (seq-to-metaprob-tuple (list 6 7 8)))
                     (list 1))
           8))))

(deftest last-1
  (testing "Last element of a metaprob list"
    (is (= (mp-last (seq-to-metaprob-list (list 1 2 3)))
           3))))

(deftest append-1
  (testing "Concatenate two metaprob lists"
    (let [l1 (seq-to-metaprob-list (list 1 2 3))
          l2 (seq-to-metaprob-list (list 7 8 9))]
      (is (= (mp-last (append l1 l2))
             9)))))
