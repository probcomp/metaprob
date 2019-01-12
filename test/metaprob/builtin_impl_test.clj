(ns metaprob.builtin-impl-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace]
            [metaprob.builtin-impl :refer :all]))

;; Procedure stuff

(deftest foreign-procedure
  (testing "create and call a foreign-procedure"
    (let [pp (make-foreign-procedure "pp" (fn [x] (+ x 1)))]
      (is (= (generate-foreign pp [6]) 7)))))

;; addresses-of

(deftest addresses-of-1
  (testing "Smoke test addresses-of"
    (let [tree {"x" {"a" {:value 1}
                     "b" {:value 2}
                     "c" {}}
                "y" {:value "d"}}
          sites (addresses-of tree)]
      (is (= (count sites) 3)))))

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

;; jmt this previously included testing sequence/sequence-to-seq,
;; which flattened a nested trace into a seq of its values. that fn is
;; gone, so this is a less interesting test now.
(deftest addresses-of-1
  (testing "addresses-of (addresses-of)"
    (let [tr    {"a" {:value 17}
                 "b" {:value 31}
                 "c" {:value {"d" {:value 71}}}}
          ;; sites (sequence/sequence-to-seq (addresses-of tr))
          sites (addresses-of tr)
          vals  (map (fn [site] (trace/trace-value tr site)) sites)
          _ (println vals)
          has? (fn [val] (some (fn [x] (= x val)) vals))]
      (has? 17)
      (has? 31)
      (has? {"d" {:value 71}}))))

(deftest sample-1
  (testing "sample-uniform smoke tests"
    (let [x (sample-uniform)
          y (sample-uniform)]
      (is (> x 0))
      (is (< x 1))
      (is (> y 0))
      (is (< y 1))
      (is (not (= x y))))))
