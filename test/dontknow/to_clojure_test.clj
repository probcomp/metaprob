(ns dontknow.to-clojure-test
  (:require [clojure.test :refer :all])
  (:require [dontknow.syntax :refer :all])
  (:require [dontknow.builtin
             :exclude [not
                       assert
                       pprint
                       list
                       and
                       or
                       first
                       rest
                       last
                       nth
                       range]])
  (:require [dontknow.to-clojure :refer :all]))

(deftest convert-1
  (testing "Smoke test for trace-to-clojure"
    (is (= (to-clojure (reconstruct-trace '("literal" "value" (7))))
           7))))

; ("definition" "rest" ("program" "body" ("application" 0 ("variable" "name" ("lookup")) 1 ("variable" "name" ("p")) 2 ("application" 0 ("variable" "name" ("list")) 1 ("literal" "value" ("rest")))) "pattern" ("tuple" 0 ("variable" "name" ("p")))) "pattern" ("variable" "name" ("rest")))

(def sample-trace-2
  '("program" "body" ("block" 0 ("if" "else" ("variable" "name" ("lst")) "then" ("application" 0 ("variable" "name" ("drop")) 1 ("application" 0 ("variable" "name" ("rest")) 1 ("variable" "name" ("lst"))) 2 ("application" 0 ("variable" "name" ("sub")) 1 ("variable" "name" ("index")) 2 ("literal" "value" (1)))) "predicate" ("application" 0 ("variable" "name" ("gt")) 1 ("variable" "name" ("index")) 2 ("literal" "value" (0))))) "pattern" ("tuple" 0 ("variable" "name" ("lst")) 1 ("variable" "name" ("index")))))

(deftest convert-2
  (testing "Smoke test for trace-to-clojure"
    (let [expr (to-clojure (reconstruct-trace sample-trace-2))]
      (is (= (first expr) 'program)))))

(deftest convert-3
  (testing "Basic test of block"
    (is (= (to-clojure (reconstruct-trace '("block"
                                            0 ("literal" "value" (1))
                                            1 ("literal" "value" (2)))))
           '(block 1 2)))))

(deftest invert-1
  (testing "Smoke test 1 for from/to-clojure"
    (let [sample '(f x y)]
      (is (= sample (to-clojure (from-clojure sample)))))))

(deftest invert-2
  (testing "Smoke test 2 for from/to-clojure"
    (let [sample '(program [x] 7 x)
          probe (to-clojure (from-clojure sample))]
      (is (= sample probe)))))
