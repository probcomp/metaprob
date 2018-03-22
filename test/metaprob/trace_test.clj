(ns metaprob.trace-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]))

; 

(deftest basic-traces
  (testing "battery of tests applied to basic traces"
    (let [tr2 (trace-from-map {"x" (new-trace 13)
                               "y" (new-trace 19)}
                              31)
          tr (trace-from-map {"a" (new-trace 17)
                              "b" (new-trace 33)
                              "c" tr2}
                             5)]
      (is (= (trace-get tr) 5))
      (is (= (count (trace-keys tr)) 3))

      (is (= (trace-get (lookup tr "a")) 17))
      (is (= (trace-get tr "b") 33))

      (is (= (trace-get tr2) 31))
      (is (= (trace-get tr2 "y") 19))

      (let [c (lookup tr "c")]
        (is (= (trace-get c) 31))
        (is (= (trace-get c "x") 13))
        (is (= (count (trace-keys c)) 2)))

      (is (= (trace-get (lookup tr '("c" "x"))) 13))
      (is (= (trace-get tr '("c" "x")) 13)))))

(deftest nil-as-trace
  (testing "see how well nil serves as a trace"
    (is (= (count (trace-keys nil)) 0))
    (is (not (trace-has? nil "a")))))

(deftest seq-as-trace
  (testing "see how well seqs serve as traces"
    (let [tr (map (fn [x] x) (list 17 33 97))]

      (is (= (trace-get tr) 17))
      (is (= (count (trace-keys tr)) 1))

      (is (= (trace-get (metaprob-rest tr)) 33))
      (is (= (trace-get (lookup tr "rest")) 33))
      (is (= (trace-get (lookup tr '("rest" "rest"))) 97))

      (is (= (length tr) 3)))))

(deftest vector-as-trace
  (testing "see how well vectors serve as traces"
    (let [tr [17 33 97]]
      (is (= (trace-get tr 0) 17))
      (is (= (trace-get tr 2) 97))

      (is (= (count (trace-keys tr)) 3))
      (is (= (length tr) 3)))))

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
      (is (= (trace-get tr) 5))
      (is (= (count (trace-keys tr)) 3))

      (is (= (trace-get (lookup tr "a")) 17))
      (is (= (trace-get tr "b") 33))

      (is (= (trace-get tr2) 31))
      (is (= (trace-get tr2 "y") 19))

      (let [c (lookup tr "c")]
        (is (= (trace-get c) 31))
        (is (= (trace-get c "x") 13))
        (is (= (count (trace-keys c)) 2)))

      (is (= (trace-get (lookup tr '("c" "x"))) 13))
      (is (= (trace-get tr '("c" "x")) 13)))))

(deftest length-1
  (testing "length smoke test"
    (is (= (length (empty-trace)) 0))
    (is (= (length (pair 0 (empty-trace))) 1))))

