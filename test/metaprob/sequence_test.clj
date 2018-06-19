(ns metaprob.sequence-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all :as trace]
            [metaprob.sequence :refer :all :as sequence]))

(deftest pair-1
  (testing "pairs and mutability"
    (is (immutable-trace? (pair 1 '())))
    (is (immutable-trace? (pair 1 (pair 2 '()))))
    (is (mutable-trace? (pair 1 (empty-trace))))
    (is (mutable-trace? (pair 1 (pair 2 (empty-trace)))))

    (is (mutable-trace? (make-mutable (pair 1 '()))))
    (is (immutable-trace? (make-immutable (pair 1 (empty-trace)))))))

(deftest pair?-1
  (testing "pair? predicate"
    (is (metaprob-pair? '(17)))
    (is (metaprob-pair? '(17 18)))
    (is (metaprob-pair? (pair 17 '())))
    (is (metaprob-pair? (make-mutable (pair 17 '()))))
    (is (not (metaprob-pair? '())))
    (is (not (metaprob-pair? nil)))
    (is (not (metaprob-pair? [17 18])))
    (is (not (metaprob-pair? (make-mutable [17 18]))))))

(deftest seq-as-trace
  (testing "see how well seqs serve as traces"
    (let [tr (map (fn [x] x) (list 17 33 97))]
      (is (metaprob-pair? tr))
      (is (= (metaprob-first tr) 17))
      (is (= (trace-get (metaprob-rest tr)) 33))
      (is (= (length tr) 3))
      (is (= (length (sequence-to-seq tr)) 3))
      (is (= (trace-get (lookup tr "rest")) 33))
      (is (= (trace-get (lookup tr '("rest" "rest"))) 97)))
    (let [tr [17 33 97]]
      (is (= (length tr) 3))
      (is (= (length (sequence-to-seq tr)) 3)))))

(deftest length-1
  (testing "length smoke test"
    (is (= (length (empty-trace)) 0))
    (is (= (length (pair 0 (empty-trace))) 1))))

(deftest list-1
  (testing "Assemble and access a mp-list"
    (is (= (sequence/metaprob-first (metaprob-list 4 5 6))
           4))))

(deftest list-2
  (testing "Assemble and access a mp-list"
    (is (= (sequence/metaprob-first (sequence/metaprob-rest (metaprob-list 4 5 6)))
           5))))

(deftest tuple2list
  (testing "Convert metaprob tuple to metaprob list"
    (let [v [5 7 11 13]
          t v]  ;was: (seq-to-mutable-tuple v)
      (is (sequence/metaprob-tuple? t))
      (let [l (to-list t)]
        (is (sequence/metaprob-pair? l))
        (let [v2 (vec (sequence/sequence-to-seq l))]
          (is (= v2 v)))))))

(deftest list2tuple
  (testing "Convert metaprob list to metaprob tuple"
    (let [l '(5 7 11 13)
          t (seq-to-mutable-list l)]
      (is (sequence/metaprob-pair? t))
      (is (trace/mutable-trace? t))
      (is (= (sequence/length t) 4))
      (let [tup (to-tuple t)]
        (is (sequence/metaprob-tuple? tup))
        (is (= (sequence/length tup) 4))
        (let [s (sequence/sequence-to-seq tup)]
          (is (seq? s))
          (is (= (count s) 4))
          (is (= (sequence/length s) 4))
          (let [v2 (vec s)]
            (is (= v2 tup))))))))

(deftest last-1
  (testing "Last element of a metaprob list"
    (is (= (metaprob-last (seq-to-mutable-list '(1 2 3)))
           3))))

(deftest append-1
  (testing "Concatenate two metaprob lists"
    (let [l1 (seq-to-mutable-list '(1 2 3))
          l2 (seq-to-mutable-list '(7 8 9))]
      (is (= (metaprob-last (append l1 l2))
             9)))))

(deftest length-2
  (testing "length smoke test"
    (let [m (seq-to-mutable-list [1 2 3 4])]
      (is (= (sequence/length m) 4))
      (is (= (sequence/length (sequence/sequence-to-seq m)) 4)))))

(deftest range-1
  (testing "range smoke test"
    (let [r (metaprob-range 5)]
      (is (= (sequence/length r) 5))
      (is (= (count (sequence/sequence-to-seq r)) 5))
      (is (= (sequence/metaprob-first r) 0))
      (is (= (metaprob-last r) 4)))))

(defn metaprob-list-contains? [s x]
  (if (trace/empty-trace? s)
    false
    (if (= x (sequence/metaprob-first s))
      true
      (metaprob-list-contains? (sequence/metaprob-rest s) x))))


(deftest list-contains-1
  (testing "smoke test metaprob-list-contains"
    (is (metaprob-list-contains? (seq-to-mutable-list '(3 5 7))
                                   5))))

(deftest list-contains-2
  (testing "smoke test metaprob-list-contains"
    (is (not (metaprob-list-contains? (seq-to-mutable-list '(3 5 7))
                                        11)))))

(deftest set-difference-1
  (testing "smoke test set-difference"
    (let [a (seq-to-mutable-list '(3 5 7))
          b (seq-to-mutable-list '(5 7 11 13))]
      (is (metaprob-list-contains? a 5) "5 in a")
      (let [a-b (set-difference a b)
            b-a (set-difference b a)]
        (is (metaprob-list-contains? a-b 3) "3 in a-b")
        (is (metaprob-list-contains? b-a 13) "13 in b-a")
        (is (not (metaprob-list-contains? a-b 7)) "7 not in a-b")
        (is (not (metaprob-list-contains? b-a 7)) "7 not in b-a")))))

(deftest nth-1
  (testing "nth smoke tests"
    (is (= (metaprob-nth (list 5 7 11) 1) 7))
    (is (= (metaprob-nth (vector 5 7 11) 1) 7))
    (is (= (metaprob-nth (metaprob-list 5 7 11) 1) 7))
    (is (= (metaprob-nth (metaprob-list 5 7 11) 0) 5))
    (is (= (metaprob-nth (metaprob-list 5 7 11) 2) 11))))

