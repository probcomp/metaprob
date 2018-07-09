(ns metaprob.trace-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]))

; 

(deftest nil-not-a-trace
  (testing "nil is not a trace"
    (is (not (trace? nil)))))

(deftest foreign-1
  (testing "tests for foreign-procedures"
    (let [ifn cons]
      (is (foreign-procedure? ifn))
      (is (not (foreign-procedure? 'foo)))
      (is (not (foreign-procedure? :foo)))
      (is (not (foreign-procedure? [1 2 3])))
      (is (not (foreign-procedure? (empty-trace))))
      (is (not (trace? ifn)))
      (is (not (trace-as-procedure? ifn))))))

(defn count-is? [tr n]
  (and (= (trace-count tr) n)
       (= (count (trace-keys tr)) n)))

(deftest tap-1
  (testing "tests for traces-as-procedures"
    (let [tr (empty-trace)
          p (trace-as-procedure tr (fn [x] x))]
      (is (trace-as-procedure? p))
      (is (trace? p))
      (is (not (trace-as-procedure? 'foo)))
      (is (not (trace-as-procedure? nil)))
      (is (count-is? p 0)))))

(deftest tap-2
  (testing "tests for empty-as-procedure"
    (let [tr '()
          p (trace-as-procedure '() (fn [x] x))]
      (is (trace-as-procedure? p))
      (is (trace? p))
      (is (count-is? p 0)))))

(deftest states-as-traces
  (testing "trace-states as traces"
    (is (immutable-trace? '()))
    (is (not (mutable-trace? '())))
    (is (immutable-trace? '(7 8)))
    (is (not (mutable-trace? '(7 8))))
    (is (immutable-trace? [7 8]))
    (is (not (mutable-trace? [7 8])))
    (is (not (immutable-trace? 'foo)))
    (is (not (mutable-trace? 'foo)))))

(deftest basic-traces
  (testing "battery of tests applied to basic traces"
    (let [tr2 (trace-from-map {"x" (new-trace 13)
                               "y" (new-trace 19)}
                              31)
          tr (trace-from-map {"a" (new-trace 17)
                              "b" (new-trace 33)
                              "c" tr2}
                             5)]
      (is (trace? tr))
      (is (mutable-trace? tr))
      (is (= (trace-get tr) 5))
      (is (count-is? tr 3))

      (is (= (trace-get (trace-subtrace tr "a")) 17))
      (is (= (trace-get tr "b") 33))

      (is (= (trace-get tr2) 31))
      (is (= (trace-get tr2 "y") 19))

      (let [c (trace-subtrace tr "c")]
        (is (= (trace-get c) 31))
        (is (= (trace-get c "x") 13))
        (is (count-is? c 2)))

      (is (= (trace-get (trace-subtrace tr '("c" "x"))) 13))
      (is (= (trace-get tr '("c" "x")) 13)))))

(deftest empty-as-trace
  (testing "see how well empty seq serves as a trace"
    (is (count-is? '() 0))
    (is (not (trace-has? '() "a")))
    (is (count-is? [] 0))
    (is (not (trace-has? [] "a")))
    (is (count-is? {} 0))
    (is (not (trace-has? {} "a")))))

(deftest mutable-1
  (testing "make-mutable-trace test"
    (is (= (trace-get (make-mutable-trace {:value 17})) 17))
    (is (= (trace-get (trace-state (make-mutable-trace {:value 17}))) 17))
    (is (= (trace-keys (make-mutable-trace [17])) '(0)))
    (is (= (count (trace-keys (make-mutable-trace '(17)))) 1))
    (is (= (trace-keys (make-mutable-trace {"foo" 17})) '("foo")))))

(deftest subtrace-1
  (testing "trace-has-subtrace?"
    (is (not (trace-has-subtrace? '() "foo")))
    (is (not (trace-has-subtrace? '() '("foo"))))
    (is (not (trace-has-subtrace? '() '("foo" "bar"))))
    (is (trace-has-subtrace? [13 17] 0))
    (is (trace-has-subtrace? [13 17] '(0)))
    (is (not (trace-has-subtrace? [13 17] 2)))
    (is (trace-has-subtrace? '(13 17) "rest"))
    (is (not (trace-has-subtrace? '(13 17) 0)))))

(deftest seq-as-trace
  (testing "see how well seqs serve as traces"
    (let [tr (map (fn [x] x) (list 17 33 97))]
      (is (= (trace-get tr) 17))
      (is (count-is? tr 1)))))    ; rest

(deftest vector-as-trace
  (testing "see how well vectors serve as traces"
    (let [tr [17 33 97]]
      (is (trace? tr))
      (is (= (trace-get tr 0) 17))
      (is (= (trace-get tr 2) 97))
      (is (count-is? tr 3)))))

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
      (is (count-is? tr 3))

      (is (= (trace-get (trace-subtrace tr "a")) 17))
      (is (= (trace-get tr "b") 33))

      (is (= (trace-get tr2) 31))
      (is (= (trace-get tr2 "y") 19))

      (let [c (trace-subtrace tr "c")]
        (is (= (trace-get c) 31))
        (is (= (trace-get c "x") 13))
        (is (count-is? c 2)))

      (is (= (trace-get (trace-subtrace tr '("c" "x"))) 13))
      (is (= (trace-get tr '("c" "x")) 13)))))

(deftest trace-1
  (testing "trace constructor, immutable"
    (let [tr (immutable-trace "x" 13)]
      (is (trace? tr))
      (is (count-is? tr 1))
      (is (trace-has? tr "x"))
      (is (= (trace-get tr "x") 13)))
    (let [tr (immutable-trace "x" 13 :value 17)]
      (is (count-is? tr 1))
      (is (trace-has? tr "x"))
      (is (= (trace-get tr) 17)))))

(deftest trace-mut
  (testing "to-mutable smoke test"
    (let [m (empty-trace)]
      (is (trace? m))
      (trace-set! m "x" 13)
      (let [tr (to-immutable m)]
        (is (count-is? tr 1))
        (is (trace-has? tr "x"))
        (is (= (trace-get tr "x") 13)))
      (trace-set! m 17)
      (let [tr (to-immutable m)]
        (is (count-is? tr 1))
        (is (trace-has? tr "x"))
        (is (= (trace-get tr) 17))))))

(deftest trace-1a
  (testing "trace constructor, mutable"
    (is (trace? (trace)))
    (let [tr (trace "x" 13)]
      (is (trace? tr))
      (is (count-is? tr 1))
      (is (trace-has? tr "x"))
      (is (= (trace-get tr "x") 13)))
    (let [tr (trace "x" 13 :value 17)]
      (is (= (trace-get tr) 17)))))

(deftest trace-2
  (testing "trace splicing"
    (let [tr (trace "x" (** (trace :value 19)))]
      (is (trace-has-subtrace? tr "x"))
      (is (= (trace-get tr "x") 19)))))

(deftest trace-set-1
  (testing "trace-set"
    (let [tr (empty-trace)]             ;mutable
      (trace-set! tr "foo" 17)
      (is (= (trace-get tr "foo") 17))
      (let [adr (list "bar" "baz")]
        (trace-set! tr adr 19)
        (is (= (trace-get tr adr) 19))))))

;; ---- locative tests

(deftest lookup-1
  (testing "locative smoke test")
  (let [tr1 (empty-trace)
        tr2 (lookup tr1 "foo")]
    (trace-set! tr2 17)
    (is (trace-get tr2) 17)
    (is (trace-get tr1 "foo") 17)))

(deftest lookup-2
  (testing "harder locative smoke test")
  (let [tr1 (empty-trace)
        tr2 (lookup tr1 "foo")
        tr3 (lookup tr2 "bar")]
    (trace-set! tr3 17)
    (is (trace-get tr3) 17)
    (is (trace-get tr2 "bar") 17)
    (is (trace-get tr1 '("foo" "bar")) 17)))

(deftest lookup-2a
  (testing "harder locative smoke test, different order")
  (let [tr1 (empty-trace)
        tr2 (lookup tr1 "foo")
        tr3 (lookup tr2 "bar")]
    (trace-set! tr2 13)
    (trace-set! tr3 17)
    (is (trace-get tr3) 17)
    (is (trace-get tr2 "bar") 17)
    (is (trace-get tr1 '("foo" "bar")) 17)
    (is (trace-get tr2) 13)
    (is (trace-get tr1 "foo") 13)))

(deftest same-1
  (testing "object comparison smoke test"
    (is (same-states? 7 7))
    (is (not (same-states? 7 8)))
    (is (same-trace-states? '(11 13) (list 11 13)))
    (is (not (same-trace-states? '(17 19) '(17))))
    (is (not (same-trace-states? '(17 19) '(17 19 23))))
    (is (same-trace-states? (trace "a" 29 "b" 31)
                            (trace "a" 29 "b" 31)))
    (is (not (same-trace-states? (trace "a" 29 "b" 31)
                                 (trace "a" 29 "b" 31 "c" 37))))
    (is (not (same-trace-states? (trace "a" 29 "b" 31 "c" 37)
                                 (trace "a" 29 "b" 31))))
    (is (not (same-trace-states? (trace "a" 29 "b" 31)
                                 (trace "a" 29 "b" 31 :value 12))))))

(deftest compare-keys-1
  (testing "compare-keys smoke tests"
    (is (= (compare-keys 7 7) 0))
    (is (< (compare-keys 7 "foo") 0))
    (is (> (compare-keys "foo" 7) 0))
    (is (< (compare-keys 7 {"foo" 7}) 0))
    (is (< (compare-keys {"abc" {:value 7}} {"foo" {:value 9}}) 0))
    (is (= (compare-keys {"abc" {:value 9}} {"abc" {:value 9}}) 0))
    (is (< (compare-keys {"abc" {:value 9}} {"abc" {:value 9} :value 5}) 0))
    (is (> (compare-keys {"abc" {:value 9}} {"abc" {:value 7}}) 0))
    (is (> (compare-keys {"abc" {:value 9} "foo" {:value 17}} {"abc" {:value 9}}) 0))))

(deftest delete-1
  (testing "delete"
    (is (empty-trace? (trace-delete {:value 5})))
    (is (same-trace-states? (trace-delete {:value 40 50 {:value 60}}) {50 {:value 60}}))
    (is (same-trace-states? (trace-delete {20 {:value 40 50 {:value 60}}} 20) {20 {50 {:value 60}}}))
    (is (same-trace-states? (trace-delete {10 {20 {:value 40 50 {:value 60}}}} (list 10 20))
                            {10 {20 {50 {:value 60}}}}))))

(deftest merge-1
  (testing "trace-merge"
    (let [tr (empty-trace)
          tr (trace-merge tr {5 {:value 55}})]
      (is (= (trace-count tr) 1) tr)
      (is (= (trace-get tr 5) 55) tr)
      (let [tr (trace-merge tr {6 {:value 66} 7 {:value 77}})]
        (is (= (trace-get tr 7) 77))
        (let [tr (trace-merge tr {:value 8})]
          (is (= (trace-get tr) 8))
          (let [tr (trace-merge tr {9 {3 {:value 33}}})]
            (is (= (trace-get tr '(9 3)) 33))
            ))))))

(deftest merge!-1
  (testing "trace-merge!"
    (let [tr (empty-trace)]
      (trace-merge! tr {5 {:value 55}})
      (is (not (empty-trace? tr)) tr)
      (is (= (trace-count tr) 1) tr)
      (is (= (trace-get tr 5) 55) tr)
      (trace-merge! tr {6 {:value 66} 7 {:value 77}})
      (is (= (trace-get tr 7) 77))
      (trace-merge! tr {:value 8})
      (is (= (trace-get tr) 8))
      (trace-merge! tr {9 {3 {:value 33}}})
      (is (= (trace-get tr '(9 3)) 33))
      )))

(deftest thaw!-1
  (testing "thaw1!"
    (let [tr (mutable-trace "a" (** (trace "immutable" 7 :value 31))
                            "b" (** (mutable-trace "mutable" 9 :value 33)))]
      (is (immutable-trace? (trace-subtrace tr "a")))
      (is (mutable-trace? (trace-subtrace tr "b")))
      (trace-thaw! tr)

      (is (mutable-trace? (trace-subtrace tr "a")))
      (is (= (trace-get tr '("a")) 31))
      (is (= (trace-get tr '("a" "immutable")) 7))

      (is (mutable-trace? (trace-subtrace tr "b")))
      (is (= (trace-get tr '("b")) 33))
      (is (= (trace-get tr '("b" "mutable")) 9)))))
