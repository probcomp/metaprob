(ns dontknow.builtin-test
  (:refer-clojure :exclude [not assert pprint and or
                            list first rest last nth range])
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]
            [dontknow.syntax :refer :all]
            [dontknow.builtin :refer :all]))

(deftest last-1
  (testing "Last element of a metaprob list"
    (is (= (last (seq-to-metaprob-list '(1 2 3)))
           3))))

(deftest append-1
  (testing "Concatenate two metaprob lists"
    (let [l1 (seq-to-metaprob-list '(1 2 3))
          l2 (seq-to-metaprob-list '(7 8 9))]
      (is (= (last (append l1 l2))
             9)))))

(deftest list-1
  (testing "Assemble and access a mp-list"
    (is (= (first (list 4 5 6))
           4))))

(deftest list-2
  (testing "Assemble and access a mp-list"
    (is (= (first (rest (list 4 5 6)))
           5))))

(deftest reification-1
  (testing "Does a program appear to be a trie?"
    (is (= (trace_get (program [] 7)) "prob prog"))))

;; The real `map` is now in the prelude, but keeping the old one
;; because hard to throw away code I worked on!

(deftest map-1
  (testing "Map over a clojure list"
    (let [foo (mp-map (fn [x] (+ x 1))
                      (list 6 7 8))]
      (is (length foo) 3)
      (is (= (nth foo 0) 7))
      (is (= (nth foo 1) 8))
      (is (= (nth foo 2) 9)))))

(deftest map-2
  (testing "Map over a metaprob list"
    (is (= (first
            (rest
             (mp-map (fn [x] (+ x 1))
                     (pair 6 (pair 7 (pair 8 (mk_nil)))))))
           8))))

(deftest map-3
  (testing "Map over a metaprob array/tuple"
    (is (= (value-at (mp-map (fn [x] (+ x 1))
                             (tuple 6 7 8))
                     '(1))
           8))))

;; Test nondeterministic primitive

(deftest flip-1
  (testing "Try doing a flip"
    (is (boolean? (flip)))
    (is (boolean? (flip 0.5)))
    (let [proposer (trace_get (lookup flip (list "custom_choice_tracing_proposer")))
          target-val true
          target (new-trie target-val)
          ;; Args to prop are: params intervene target output
          result (proposer (mk_nil) (mk_nil) target (mk_nil))
          [val score] (metaprob-list-to-seq result)]
      (prn [val score])
      (is (= val target-val))
      (is (> score -2))
      (is (< score 0)))))


