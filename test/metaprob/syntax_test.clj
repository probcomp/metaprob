(ns metaprob.syntax-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.state :as state]
            [metaprob.syntax :refer :all]
            [metaprob.builtin-impl :as impl]))

(deftest gen-1
  (testing "Smoke test for gen macro"
    (is (= ((gen [x] x) 1) 1))))

(deftest gen-2
  (testing "ara procedures traces?"
    (is (trace? (gen [x] x)))))

(deftest gen-3
  (testing "are procedures named?"
    ;; Name would be something like "-1239293465-foo"
    (is (.contains (impl/procedure-name (named-generator foo [x] x)) "foo"))))

(deftest gen-4
  (testing "trace literal"
    (testing "maps"
      (is (trace? ((gen [] {}))))
      (is (trace? ((gen [] {"a" {}}))))
      (testing "nesting"
        (is (trace? (trace-subtrace ((gen [] {"subtrace" {}}))
                                    "subtrace")))))
    (testing "lists"
      (is (trace? ((gen [] (list)))))
      (is (trace? ((gen [] (list 1 2 3)))))
      (testing "nesting"
        (is (trace? (trace-subtrace ((gen [] (list 1 (list 2))))
                                    state/rest-marker)))))))

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
    (is (= (block (define foo (gen [x] x)) (foo 7)) 7))))

(deftest block-mixed-1
  (testing "Smoke test for block macro"
    (is (= (block (define foo (gen [x] x)) (define bar 7) (foo bar)) 7))))

(deftest block-1
  (testing "Define inside block is local"
    (is (= (block (define foo 2)
                  (block (define foo 3) foo)
                  foo)
           2))))

(deftest block-2
  (testing "Function definition becomes letfn"
    (is (= (block (define foo (gen [x] (if (= x 4) 5 (foo 4))))
                  (foo 3))
           5))))

(deftest block-3
  (testing "Pattern in let"
    (is (= (block (define [a b] [1 2])
                  b)
           2))))

(deftest block-3a
  (testing "Pattern in let"
    (is (= ((gen []
                     (define [v _] [6 7])
                     v))
           6))))
           
(deftest block-4
  (testing "Nested pattern in let"
    (is (= (block (define [a [b c d]] [1 [2 3 4]])
                  c)
           3))))

(deftest define-1
  (testing "Basic definition"
    (let [form '(define foo 17)]
      (binding [*ns* (find-ns 'metaprob.syntax)]
        (eval form)
        (is (= (eval 'foo) 17))))))

(deftest define-2
  (testing "definition where value is a procedure"
    (let [form '(define foo (gen [x] x))]
      (binding [*ns* (find-ns 'metaprob.syntax)]
        (eval form)
        (let [proc (eval 'foo)]
          (is (= (proc 17) 17) proc))))))
