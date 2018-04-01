(ns metaprob.to-clojure-test
  (:require [clojure.test :refer :all])
  (:require [metaprob.trace :refer :all])
  (:require [metaprob.builtin-impl :refer :all])
  (:require [metaprob.syntax :as syntax])
  (:require [metaprob.to-clojure :refer :all]))

(deftest convert-1
  (testing "Smoke test for trace-to-clojure"
    (let [tr (reconstruct-trace '("literal" "value" (7)))]
      (is (trace? tr))
      (is (= (to-clojure tr) 7)))))

; ("definition" "rest" ("program" "body" ("application" 0 ("variable" "name" ("lookup")) 1 ("variable" "name" ("p")) 2 ("application" 0 ("variable" "name" ("list")) 1 ("literal" "value" ("rest")))) "pattern" ("tuple" 0 ("variable" "name" ("p")))) "pattern" ("variable" "name" ("rest")))

(def sample-trace-2
  '("program" "body" ("block" 0 ("if" "else" ("variable" "name" ("lst")) "then" ("application" 0 ("variable" "name" ("drop")) 1 ("application" 0 ("variable" "name" ("rest")) 1 ("variable" "name" ("lst"))) 2 ("application" 0 ("variable" "name" ("sub")) 1 ("variable" "name" ("index")) 2 ("literal" "value" (1)))) "predicate" ("application" 0 ("variable" "name" ("gt")) 1 ("variable" "name" ("index")) 2 ("literal" "value" (0))))) "pattern" ("tuple" 0 ("variable" "name" ("lst")) 1 ("variable" "name" ("index")))))

(defn strip-export [x]
  (if (and (seq? x)
           (not (empty? x))
           (= (first x) 'opaque))
    (cons 'gen (rest x))
    x))

(deftest convert-2
  (testing "Smoke test for trace-to-clojure"
    (let [expr (to-clojure (reconstruct-trace sample-trace-2))
          expr2 (strip-export expr)
          op (first expr2)]
      (is (= op 'gen)))))

(deftest convert-3
  (testing "Basic test of block"
    (is (= (to-clojure (reconstruct-trace '("block"
                                            0 ("literal" "value" (1))
                                            1 ("literal" "value" (2)))))
           '(block 1 2)))))

(deftest invert-1
  (testing "Smoke test 1 for from/to-clojure"
    (let [sample '(f x y)]
      (is (= sample (to-clojure (syntax/from-clojure sample)))))))

(deftest invert-2
  (testing "Smoke test 2 for from/to-clojure"
    (let [sample '(gen [x] 7 x)
          tr (syntax/from-clojure sample)
          probe (strip-export (to-clojure tr))]
      (is (= sample probe)))))
