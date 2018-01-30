(ns dontknow.propose-and-trace-choices-test
  (:refer-clojure :exclude [not assert pprint and or
                            list first rest last nth range])
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]
            [dontknow.syntax :refer :all]
            [dontknow.builtin :as builtin :exclude [first rest]]
            [dontknow.propose-and-trace-choices :refer :all]))

(defn ez-eval [x]
  (ptc_eval (from-clojure x)
            nil
            (builtin/mk_nil)
            (builtin/mk_nil)
            (builtin/mk_nil)))

(deftest smoke-1
  (testing "Interpret a literal expression"
    (is (= (ez-eval 3)
           3))))

(deftest smoke-2
  (testing "Interpret a variable"
    (is (= (ez-eval 'first)
           builtin/first))))

