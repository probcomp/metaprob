(ns metaprob.trace-test
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]))

(deftest nil-not-a-trace
  (testing "nil is not a trace"
    (is (not (trace/trace? nil)))))

(deftest basic-traces
  (testing "battery of tests applied to basic traces"
    (let [tr2 {"x" {:value 13}
               "y" {:value 19}
               :value 31}
          tr {"a" {:value 17}
              "b" {:value 39}
              "c" tr2
              :value 5}]
      (is (trace/trace? tr))
      (is (= (trace/trace-value tr) 5))

      (is (= (trace/trace-value (trace/trace-subtrace tr "a")) 17))
      (is (= (trace/trace-value tr "b") 39))

      (is (= (trace/trace-value tr2) 31))
      (is (= (trace/trace-value tr2 "y") 19))

      (let [c (trace/trace-subtrace tr "c")]
        (is (= (trace/trace-value c) 31))
        (is (= (trace/trace-value c "x") 13)))

      (is (= (trace/trace-value (trace/trace-subtrace tr '("c" "x"))) 13))
      (is (= (trace/trace-value tr '("c" "x")) 13)))))

(deftest empty-as-trace
  (testing "see how well empty seq serves as a trace"
    (is (not (trace/trace-has-subtrace? {} "a")))
    (is (not (trace/trace-has-value? {})))))

(deftest subtrace-1
  (testing "trace-has-subtrace?"
    (is (not (trace/trace-has-subtrace? {} "foo")))
    (is (not (trace/trace-has-subtrace? {} '("foo"))))
    (is (not (trace/trace-has-subtrace? {} '("foo" "bar"))))
    (is (trace/trace-has-subtrace? {:a {}} '(:a)))
    (is (trace/trace-has-subtrace? {:a {:b {}}} '(:a :b)))))

(deftest map-as-trace
  (testing "see how maps serve as traces"
    (let [new-trace (fn [x] {:value x})
          trace-from-map (fn [x val]
                           (assoc x :value val))
          tr2 (trace-from-map {"x" (new-trace 13)
                               "y" (new-trace 19)}
                              31)
          tr (trace-from-map {"a" (new-trace 17)
                              "b" (new-trace 33)
                              "c" tr2}
                             5)]
      (is (= (trace/trace-value tr) 5))

      (is (= (trace/trace-value (trace/trace-subtrace tr "a")) 17))
      (is (= (trace/trace-value tr "b") 33))

      (is (= (trace/trace-value tr2) 31))
      (is (= (trace/trace-value tr2 "y") 19))

      (let [c (trace/trace-subtrace tr "c")]
        (is (= (trace/trace-value c) 31))
        (is (= (trace/trace-value c "x") 13)))

      (is (= (trace/trace-value (trace/trace-subtrace tr '("c" "x"))) 13))
      (is (= (trace/trace-value tr '("c" "x")) 13)))))

(deftest compare-keys-1
  (testing "compare-keys smoke tests"
    (is (= (trace/compare-keys 7 7) 0))
    (is (< (trace/compare-keys 7 "foo") 0))
    (is (> (trace/compare-keys "foo" 7) 0))
    (is (< (trace/compare-keys 7 {"foo" 7}) 0))
    (is (< (trace/compare-keys {"abc" {:value 7}} {"foo" {:value 9}}) 0))
    (is (= (trace/compare-keys {"abc" {:value 9}} {"abc" {:value 9}}) 0))
    (is (< (trace/compare-keys {"abc" {:value 9}} {"abc" {:value 9} :value 5}) 0))
    (is (> (trace/compare-keys {"abc" {:value 9}} {"abc" {:value 7}}) 0))
    (is (> (trace/compare-keys {"abc" {:value 9} "foo" {:value 17}} {"abc" {:value 9}}) 0))))

(deftest merge-1
  (testing "trace-merge"
    (let [tr {}
          tr (trace/trace-merge tr {5 {:value 55}})]
      (is (= (count tr) 1) tr)
      (is (= (trace/trace-value tr 5) 55) tr)
      (let [tr (trace/trace-merge tr {6 {:value 66} 7 {:value 77}})]
        (is (= (trace/trace-value tr 7) 77))
        (let [tr (trace/trace-merge tr {:value 8})]
          (is (= (trace/trace-value tr) 8))
          (let [tr (trace/trace-merge tr {9 {3 {:value 33}}})]
            (is (= (trace/trace-value tr '(9 3)) 33))))))))
