(ns metaprob.metacirc.query-test
  (:refer-clojure :exclude [not assert pprint and or
                            list first rest last nth range])
  (:require [clojure.test :refer :all]
            [metaprob.environment :as environment]
            [metaprob.trace :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.builtin :as builtin]
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

;; N.b. this will reify the program to get stuff to eval

(deftest binding-1
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-apply (program [x] x) 5)
           5))))

(deftest binding-2
  (testing "Bind a variable locally to a value (eval)"
    (is (= (ez-eval '((program [x] x) 5))
           5))))

