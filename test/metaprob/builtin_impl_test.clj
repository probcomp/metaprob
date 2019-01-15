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

(deftest addresses-of-2
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
