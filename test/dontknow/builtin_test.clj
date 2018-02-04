(ns dontknow.builtin-test
  (:require [clojure.test :refer :all]
            [dontknow.trace :refer :all]
            [dontknow.syntax :refer :all]
            [dontknow.builtin :as b]))

(deftest last-1
  (testing "Last element of a metaprob list"
    (is (= (b/last (b/seq-to-metaprob-list '(1 2 3)))
           3))))

(deftest append-1
  (testing "Concatenate two metaprob lists"
    (let [l1 (b/seq-to-metaprob-list '(1 2 3))
          l2 (b/seq-to-metaprob-list '(7 8 9))]
      (is (= (b/last (b/append l1 l2))
             9)))))

(deftest list-1
  (testing "Assemble and access a mp-list"
    (is (= (b/first (b/list 4 5 6))
           4))))

(deftest list-2
  (testing "Assemble and access a mp-list"
    (is (= (b/first (b/rest (b/list 4 5 6)))
           5))))

(deftest reification-1
  (testing "Does a program appear to be a trie?"
    (is (= (b/trace_get (program [] 7)) "prob prog"))))

;; The real `map` is now in the prelude, but keeping the old one
;; because hard to throw away code I worked on!

(deftest map-1
  (testing "Map over a clojure list"
    (let [foo (b/mp-map (fn [x] (+ x 1))
                        (b/list 6 7 8))]
      (is (b/length foo) 3)
      (is (= (b/nth foo 0) 7))
      (is (= (b/nth foo 1) 8))
      (is (= (b/nth foo 2) 9)))))

(deftest map-2
  (testing "Map over a metaprob list"
    (is (= (b/first
            (b/rest
             (b/mp-map (fn [x] (+ x 1))
                       (b/pair 6 (b/pair 7 (b/pair 8 (b/mk_nil)))))))
           8))))

(deftest map-3
  (testing "Map over a metaprob array/tuple"
    (is (= (value-at (b/mp-map (fn [x] (+ x 1))
                               (tuple 6 7 8))
                     '(1))
           8))))

;; Test nondeterministic primitive

(deftest flip-1
  (testing "Try doing a flip"
    (is (boolean? (b/flip)))
    (is (boolean? (b/flip 0.5)))
    (let [proposer (b/trace_get (b/lookup b/flip (b/list "custom_choice_tracing_proposer")))
          target-val true
          target (new-trie target-val)
          ;; Args to prop are: params intervene target output
          result (proposer (b/mk_nil) (b/mk_nil) target (b/mk_nil))
          [val score] (b/metaprob-list-to-seq result)]
      (is (= val target-val))
      (is (> score -2))
      (is (< score 0)))))

;; trace_sites

(deftest trace_sites-1
  (testing "Smoke test trace_sites"
    (let [tree (trie-from-map {"x" (trie-from-map {"a" (new-trie 1)
                                                   "b" (new-trie 2)
                                                   "c" (new-trie)})
                               "y" (new-trie "d")})
          sites (b/trace_sites tree)]
      (is (= (b/length sites) 3)))))

                              
