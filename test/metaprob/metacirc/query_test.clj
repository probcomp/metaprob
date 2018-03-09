(ns metaprob.metacirc.query-test
  (:require [clojure.test :refer :all]
            [metaprob.environment :as environment]
            [metaprob.trace :refer :all]
            [metaprob.syntax :as syntax :refer :all]
            [metaprob.builtin :as builtin :refer [metaprob-nth]]
            [metaprob.metacirc.query :refer :all]))

(defn mk_nil [] (builtin/empty-trace))

(defn ez-apply [prob-prog & args]
  (let [[value score]
        (builtin/metaprob-collection-to-seq
         (query prob-prog
                (builtin/seq-to-metaprob-tuple args)
                (mk_nil) (mk_nil) (mk_nil)))]
    value))

(deftest apply-1
  (testing "Apply a probprog to no args"
    (is (builtin/empty-trace?
         (ez-apply builtin/empty-trace)))))

(deftest apply-2
  (testing "Apply a probprog to one arg"
    (is (= (ez-apply builtin/sub 7)
           -7))))

(defn ez-eval [x]
  (let [[value score]
        (builtin/metaprob-collection-to-seq
         (ptc_eval (from-clojure x)
                   (environment/make-top-level-env 'metaprob.metacirc.query)
                   (mk_nil)
                   (mk_nil)
                   (mk_nil)))]
    value))

(deftest smoke-1
  (testing "Interpret a literal expression"
    (is (= (ez-eval 3)
           3))))

(deftest smoke-2
  (testing "Interpret a variable"
    (is (= (ez-eval 'first)
           builtin/first))))

;; N.b. this will reify the probprog to get stuff to eval

(deftest binding-1
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-apply (probprog [x] x) 5)
           5))))

(deftest binding-2
  (testing "Bind a variable locally to a value (eval)"
    (is (= (ez-eval '((probprog [x] x) 5))
           5))))

;; Export a probprog i.e. use 'foreign' (clojure) version rather than
;; trying to compile the 'native' version (source code)

(deftest export-1
  (testing "export a probprog"
    (let [x 5
          m1 (probprog [] x)
          m2 (builtin/export-probprog m1)]
      (is (= (m2) (m1))))))

;; Lift a generate method up to a query method

(deftest lift-1
  (testing "lift a generate method up to a query method"
    (let [m (builtin/export-probprog
             (probprog [argseq i t o]
                       (define [x y] argseq)
                       (tuple (+ x 1) 19)))
          l (builtin/make-lifted-probprog "testing" m)]
      (is (= (l 17 "z") 18)))))
