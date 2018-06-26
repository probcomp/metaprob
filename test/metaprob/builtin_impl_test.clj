(ns metaprob.builtin-impl-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace :refer [trace]]
            [metaprob.sequence :as sequence]
            [metaprob.builtin-impl :refer :all]))

;; Procedure stuff

(deftest foreign-procedure
  (testing "create and call a foreign-procedure"
    (let [pp (make-foreign-procedure "pp" (fn [x] (+ x 1)))]
      (is (= (generate-foreign pp [6]) 7)))))

;; addresses-of

(deftest addresses-of-1
  (testing "Smoke test addresses-of"
    (let [tree (trace/trace-from-map
                {"x" (trace/trace-from-map {"a" (trace/new-trace 1)
                                            "b" (trace/new-trace 2)
                                            "c" (trace/empty-trace)})
                 "y" (trace/new-trace "d")})
          sites (addresses-of tree)]
      (is (= (sequence/length sites) 3)))))

;; match-bind!

;; (deftest match-bind-1
;;   (testing "match-bind! smoke"
;;     (let [env (make-env ... what a pain in the ass ...)]
;;       (match-bind! (from-clojure '[a b])
;;                   [1 2]
;;                   env)
;;       (is (= (env-lookup env "a") 1))
;;       (is (= (env-lookup env "b") 2)))))

;; Does list s contain element x?

;(deftest hairy-key-1
;  (testing "does it work to use a procedure name as a trace"
;    (let [pp (gen [x] x)
;          pt pp
;          key (trace/trace-get pt "name")
;          tr (trace/trace-from-map {key (trace/new-trace 17)})]
;      (is (= (trace/trace-get tr key) 17)))))

(deftest addresses-of-1
  (testing "addresses-of (addresses-of)"
    (let [tr (trace/trace-from-map {"a" (trace/new-trace 17)
                                    "b" (trace/new-trace 31)
                                    "c" (trace/trace-from-map {"d" (trace/new-trace 71)})})
          sites (sequence/sequence-to-seq (addresses-of tr))
          vals  (map (fn [site] (trace/trace-get tr site)) sites)
          has? (fn [val] (some (fn [x] (= x val)) vals))]
      (has? 17)
      (has? 71))))

(deftest sample-1
  (testing "sample-uniform smoke tests"
    (let [x (sample-uniform)
          y (sample-uniform)]
      (is (> x 0))
      (is (< x 1))
      (is (> y 0))
      (is (< y 1))
      (is (not (= x y))))))

