(ns metaprob.sequence-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]
            [metaprob.sequence :as sequence]))

(deftest pair-1
  (testing "pairs and mutability"
    (is (trace/immutable-trace? (sequence/pair 1 '())))
    (is (trace/immutable-trace? (sequence/pair 1 (sequence/pair 2 '()))))
    (is (trace/mutable-trace? (sequence/pair 1 (trace/empty-trace))))
    (is (trace/mutable-trace? (sequence/pair 1 (sequence/pair 2 (trace/empty-trace)))))

    (is (trace/mutable-trace? (trace/to-mutable (sequence/pair 1 '()))))
    (is (trace/immutable-trace? (trace/to-immutable (sequence/pair 1 (trace/empty-trace)))))))

(deftest pair?-1
  (testing "pair? predicate"
    (is (sequence/metaprob-pair? '(17)))
    (is (sequence/metaprob-pair? '(17 18)))
    (is (sequence/metaprob-pair? (sequence/pair 17 '())))
    (is (sequence/metaprob-pair? (trace/to-mutable (sequence/pair 17 '()))))
    (is (not (sequence/metaprob-pair? '())))
    (is (not (sequence/metaprob-pair? nil)))
    (is (not (sequence/metaprob-pair? [17 18])))
    (is (not (sequence/metaprob-pair? (trace/to-mutable [17 18]))))))

(deftest seq-as-trace
  (testing "see how well seqs serve as traces"
    (let [tr (map (fn [x] x) (list 17 33 97))]
      (is (sequence/metaprob-pair? tr))
      (is (= (sequence/metaprob-first tr) 17))
      (is (= (trace/trace-get (sequence/metaprob-rest tr)) 33))
      (is (= (sequence/length tr) 3))
      (is (= (sequence/length (sequence/sequence-to-seq tr)) 3))
      (is (= (trace/trace-get tr "rest") 33))
      (is (= (trace/trace-get tr '("rest" "rest")) 97)))
    (let [tr [17 33 97]]
      (is (= (sequence/length tr) 3))
      (is (= (sequence/length (sequence/sequence-to-seq tr)) 3)))))

(deftest length-1
  (testing "length smoke test"
    (is (= (sequence/length (trace/empty-trace)) 0))
    (is (= (sequence/length (sequence/pair 0 (trace/empty-trace))) 1))))

(deftest list-1
  (testing "Assemble and access a mp-list"
    (is (= (sequence/metaprob-first (sequence/metaprob-list 4 5 6))
           4))))

(deftest list-2
  (testing "Assemble and access a mp-list"
    (is (= (sequence/metaprob-first (sequence/metaprob-rest (sequence/metaprob-list 4 5 6)))
           5))))

(deftest tuple2list
  (testing "Convert metaprob tuple to metaprob list"
    (let [v [5 7 11 13]
          t v]  ;was: (seq-trace/to-mutable-tuple v)
      (is (sequence/tuple? t))
      (let [l (sequence/to-list t)]
        (is (sequence/metaprob-pair? l))
        (let [v2 (vec (sequence/sequence-to-seq l))]
          (is (= v2 v)))))))

(deftest list2tuple
  (testing "Convert metaprob list to metaprob tuple"
    (let [l '(5 7 11 13)
          t (sequence/seq-to-mutable-list l)]
      (is (sequence/metaprob-pair? t))
      (is (trace/mutable-trace? t))
      (is (= (sequence/length t) 4))
      (let [tup (sequence/to-tuple t)]
        (is (sequence/tuple? tup))
        (is (= (sequence/length tup) 4))
        (let [s (sequence/sequence-to-seq tup)]
          (is (seq? s))
          (is (= (count s) 4))
          (is (= (sequence/length s) 4))
          (let [v2 (vec s)]
            (is (= v2 tup))))))))

(deftest last-1
  (testing "Last element of a metaprob list"
    (is (= (sequence/metaprob-last (sequence/seq-to-mutable-list '(1 2 3)))
           3))))

(deftest append-1
  (testing "Concatenate two metaprob lists"
    (let [l1 (sequence/seq-to-mutable-list '(1 2 3))
          l2 (sequence/seq-to-mutable-list '(7 8 9))]
      (is (= (sequence/metaprob-last (sequence/append l1 l2))
             9)))))

(deftest length-2
  (testing "length smoke test"
    (let [m (sequence/seq-to-mutable-list [1 2 3 4])]
      (is (= (sequence/length m) 4))
      (is (= (sequence/length (sequence/sequence-to-seq m)) 4)))))

(deftest range-1
  (testing "range smoke test"
    (let [r (sequence/metaprob-range 5)]
      (is (= (sequence/length r) 5))
      (is (= (count (sequence/sequence-to-seq r)) 5))
      (is (= (sequence/metaprob-first r) 0))
      (is (= (sequence/metaprob-last r) 4)))))

(defn metaprob-list-contains? [s x]
  (if (trace/empty-trace? s)
    false
    (if (= x (sequence/metaprob-first s))
      true
      (metaprob-list-contains? (sequence/metaprob-rest s) x))))


(deftest list-contains-1
  (testing "smoke test metaprob-list-contains"
    (is (metaprob-list-contains? (sequence/seq-to-mutable-list '(3 5 7))
                                   5))))

(deftest list-contains-2
  (testing "smoke test metaprob-list-contains"
    (is (not (metaprob-list-contains? (sequence/seq-to-mutable-list '(3 5 7))
                                        11)))))

(deftest set-difference-1
  (testing "smoke test set-difference"
    (let [a (sequence/seq-to-mutable-list '(3 5 7))
          b (sequence/seq-to-mutable-list '(5 7 11 13))]
      (is (metaprob-list-contains? a 5) "5 in a")
      (let [a-b (sequence/set-difference a b)
            b-a (sequence/set-difference b a)]
        (is (metaprob-list-contains? a-b 3) "3 in a-b")
        (is (metaprob-list-contains? b-a 13) "13 in b-a")
        (is (not (metaprob-list-contains? a-b 7)) "7 not in a-b")
        (is (not (metaprob-list-contains? b-a 7)) "7 not in b-a")))))

(deftest nth-1
  (testing "nth smoke tests"
    (is (= (sequence/metaprob-nth (list 5 7 11) 1) 7))
    (is (= (sequence/metaprob-nth (vector 5 7 11) 1) 7))
    (is (= (sequence/metaprob-nth (sequence/metaprob-list 5 7 11) 1) 7))
    (is (= (sequence/metaprob-nth (sequence/metaprob-list 5 7 11) 0) 5))
    (is (= (sequence/metaprob-nth (sequence/metaprob-list 5 7 11) 2) 11))))
