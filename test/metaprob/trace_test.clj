(ns metaprob.trace-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]))

; 

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

      (is (= (trace-get (lookup tr "a")) 17))
      (is (= (trace-get tr "b") 33))

      (is (= (trace-get tr2) 31))
      (is (= (trace-get tr2 "y") 19))

      (let [c (lookup tr "c")]
        (is (= (trace-get c) 31))
        (is (= (trace-get c "x") 13))
        (is (count-is? c 2)))

      (is (= (trace-get (lookup tr '("c" "x"))) 13))
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
    (is (= (trace-get (make-mutable-trace '(17))) 17))))

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

      (is (= (trace-get (lookup tr "a")) 17))
      (is (= (trace-get tr "b") 33))

      (is (= (trace-get tr2) 31))
      (is (= (trace-get tr2 "y") 19))

      (let [c (lookup tr "c")]
        (is (= (trace-get c) 31))
        (is (= (trace-get c "x") 13))
        (is (count-is? c 2)))

      (is (= (trace-get (lookup tr '("c" "x"))) 13))
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
  (testing "make-mutable smoke test"
    (let [m (empty-trace)]
      (is (trace? m))
      (trace-set! m "x" 13)
      (let [tr (make-immutable m)]
        (is (count-is? tr 1))
        (is (trace-has? tr "x"))
        (is (= (trace-get tr "x") 13)))
      (trace-set! m 17)
      (let [tr (make-immutable m)]
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
