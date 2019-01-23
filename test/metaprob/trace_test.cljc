(ns metaprob.trace-test
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :as trace]))

(deftest nil-not-a-trace
  (testing "nil is not a trace"
    (is (not (trace/trace? nil)))))

(deftest foreign-1
  (testing "tests for foreign-procedures"
    (let [ifn cons]
      ;; (is (trace/foreign-procedure? ifn))
      ;; (is (not (trace/foreign-procedure? 'foo)))
      ;; (is (not (trace/foreign-procedure? :foo)))
      ;; (is (not (trace/foreign-procedure? [1 2 3])))
      ;; (is (not (trace/foreign-procedure? (trace/empty-trace))))
      (is (not (trace/trace? ifn)))
      ;; (is (not (trace/trace-as-procedure? ifn)))
      )))

(defn count-is? [tr n]
  (and (= (count tr) n)
       (= (count (trace/trace-keys tr)) n)))

;; (deftest tap-1
;;   (testing "tests for traces-as-procedures"
;;     (let [tr {}
;;           p (trace/trace-as-procedure tr (fn [x] x))]
;;       (is (trace/trace-as-procedure? p))
;;       (is (trace/trace? p))
;;       (is (not (trace/trace-as-procedure? 'foo)))
;;       (is (not (trace/trace-as-procedure? nil)))
;;       (is (count-is? p 0)))))

;; (deftest tap-2
;;   (testing "tests for empty-as-procedure"
;;     (let [tr '()
;;           p (trace/trace-as-procedure '() (fn [x] x))]
;;       (is (trace/trace-as-procedure? p))
;;       (is (trace/trace? p))
;;       (is (count-is? p 0)))))

;; (deftest states-as-traces
;;   (testing "trace-states as traces"
;;     (is (trace/immutable-trace? '()))
;;     (is (not (trace/mutable-trace? '())))
;;     (is (trace/immutable-trace? '(7 8)))
;;     (is (not (trace/mutable-trace? '(7 8))))
;;     (is (trace/immutable-trace? [7 8]))
;;     (is (not (trace/mutable-trace? [7 8])))
;;     (is (not (trace/immutable-trace? 'foo)))
;;     (is (not (trace/mutable-trace? 'foo)))))

(deftest basic-traces
  (testing "battery of tests applied to basic traces"
    (let [tr2 {"x" {:value 13}
               "y" {:value 19}
               :value 31}
          tr {"a" {:value 17}
              "b" {:value 39}
              "c" {:value tr2}
              :value 5}]
      (is (trace/trace? tr))
      (is (= (trace/trace-value tr) 5))
      (is (count-is? tr 3))

      (is (= (trace/trace-value (trace/trace-subtrace tr "a")) 17))
      (is (= (trace/trace-value tr "b") 33))

      (is (= (trace/trace-value tr2) 31))
      (is (= (trace/trace-value tr2 "y") 19))

      (let [c (trace/trace-subtrace tr "c")]
        (is (= (trace/trace-value c) 31))
        (is (= (trace/trace-value c "x") 13))
        (is (count-is? c 2)))

      (is (= (trace/trace-value (trace/trace-subtrace tr '("c" "x"))) 13))
      (is (= (trace/trace-value tr '("c" "x")) 13)))))

(deftest empty-as-trace
  (testing "see how well empty seq serves as a trace"
    (is (count-is? '() 0))
    ;; (is (not (trace/trace-has? '() "a")))
    (is (count-is? [] 0))
    ;; (is (not (trace/trace-has? [] "a")))
    (is (count-is? {} 0))
    ;;(is (not (trace/trace-has? {} "a")))
    ))


;; (deftest mutable-1
;;   (testing "make-mutable-trace test"
;;     (is (= (trace/trace-get (trace/make-mutable-trace {:value 17})) 17))
;;     (is (= (trace/trace-get (trace/trace-state (trace/make-mutable-trace {:value 17}))) 17))
;;     (is (= (trace/trace-keys (trace/make-mutable-trace [17])) '(0)))
;;     (is (= (count (trace/trace-keys (trace/make-mutable-trace '(17)))) 1))
;;     (is (= (trace/trace-keys (trace/make-mutable-trace {"foo" 17})) '("foo")))))

(deftest subtrace-1
  (testing "trace-has-subtrace?"
    (is (not (trace/trace-has-subtrace? '() "foo")))
    (is (not (trace/trace-has-subtrace? '() '("foo"))))
    (is (not (trace/trace-has-subtrace? '() '("foo" "bar"))))
    (is (trace/trace-has-subtrace? [13 17] 0))
    (is (trace/trace-has-subtrace? [13 17] '(0)))
    (is (not (trace/trace-has-subtrace? [13 17] 2)))
    (is (trace/trace-has-subtrace? '(13 17) "rest"))
    (is (not (trace/trace-has-subtrace? '(13 17) 0)))))

;; jmt ?? what's correct here?
;; (deftest seq-as-trace
;;   (testing "see how well seqs serve as traces"
;;     (let [tr (map (fn [x] x) (list 17 33 97))]
;;       (is (= (trace/trace-value tr) 17))
;;       (is (count-is? tr 1)))))

;; jmt ?? what's correct here?
;; (deftest vector-as-trace
;;   (testing "see how well vectors serve as traces"
;;     (let [tr [17 33 97]]
;;       (is (trace/trace? tr))
;;       (is (= (trace/trace-value tr 0) 17))
;;       (is (= (trace/trace-value tr 2) 97))
;;       (is (count-is? tr 3)))))

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
      (is (count-is? tr 3))

      (is (= (trace/trace-value (trace/trace-subtrace tr "a")) 17))
      (is (= (trace/trace-value tr "b") 33))

      (is (= (trace/trace-value tr2) 31))
      (is (= (trace/trace-value tr2 "y") 19))

      (let [c (trace/trace-subtrace tr "c")]
        (is (= (trace/trace-value c) 31))
        (is (= (trace/trace-value c "x") 13))
        (is (count-is? c 2)))

      (is (= (trace/trace-value (trace/trace-subtrace tr '("c" "x"))) 13))
      (is (= (trace/trace-value tr '("c" "x")) 13)))))

;; (deftest trace-1
;;   (testing "trace constructor, immutable"
;;     (let [tr (trace/immutable-trace "x" 13)]
;;       (is (trace/trace? tr))
;;       (is (count-is? tr 1))
;;       (is (trace/trace-has? tr "x"))
;;       (is (= (trace/trace-get tr "x") 13)))
;;     (let [tr (trace/immutable-trace "x" 13 :value 17)]
;;       (is (count-is? tr 1))
;;       (is (trace/trace-has? tr "x"))
;;       (is (= (trace/trace-get tr) 17)))))

;; (deftest trace-mut
;;   (testing "to-mutable smoke test"
;;     (let [m (trace/empty-trace)]
;;       (is (trace/trace? m))
;;       (trace/trace-set! m "x" 13)
;;       (let [tr (trace/to-immutable m)]
;;         (is (count-is? tr 1))
;;         (is (trace/trace-has? tr "x"))
;;         (is (= (trace/trace-get tr "x") 13)))
;;       (trace/trace-set! m 17)
;;       (let [tr (trace/to-immutable m)]
;;         (is (count-is? tr 1))
;;         (is (trace/trace-has? tr "x"))
;;         (is (= (trace/trace-get tr) 17))))))

;; (deftest trace-1a
;;   (testing "trace constructor, mutable"
;;     (is (trace/trace? (trace/trace)))
;;     (let [tr (trace/trace "x" 13)]
;;       (is (trace/trace? tr))
;;       (is (count-is? tr 1))
;;       (is (trace/trace-has? tr "x"))
;;       (is (= (trace/trace-get tr "x") 13)))
;;     (let [tr (trace/trace "x" 13 :value 17)]
;;       (is (= (trace/trace-get tr) 17)))))

;; (deftest trace-2
;;   (testing "trace splicing"
;;     (let [tr (trace/trace "x" (trace/** (trace/trace :value 19)))]
;;       (is (trace/trace-has-subtrace? tr "x"))
;;       (is (= (trace/trace-get tr "x") 19)))))

;; (deftest trace-set-1
;;   (testing "trace-set"
;;     (let [tr (trace/empty-trace)]             ;mutable
;;       (trace/trace-set! tr "foo" 17)
;;       (is (= (trace/trace-get tr "foo") 17))
;;       (let [adr (list "bar" "baz")]
;;         (trace/trace-set! tr adr 19)
;;         (is (= (trace/trace-get tr adr) 19))))))

;; (deftest trace-yielding-pair
;;   (testing "trace-yielding-pair"
;;     (is (= (trace/trace :value 7 "rest" (trace/** '())) '(7)))
;;     (is (= (trace/trace :value 7 "rest" (trace/** '(8))) '(7 8)))
;;     (is (= (trace/trace :value 7 "rest" (trace/** {})) '(7)))
;;     (is (= (trace/trace :value 7 "rest" (trace/** [])) '(7)))
;;     (is (map? (trace/trace :value 7 "rest" {})))
;;     (is (map? (trace/trace :value 7 "rest" (trace/** '(8)) "frass" 27)))))


;; (deftest same-1
;;   (testing "object comparison smoke test"
;;     (is (trace/same-states? 7 7))
;;     (is (not (trace/same-states? 7 8)))
;;     (is (trace/same-trace-states? '(11 13) (list 11 13)))
;;     (is (not (trace/same-trace-states? '(17 19) '(17))))
;;     (is (not (trace/same-trace-states? '(17 19) '(17 19 23))))
;;     (is (trace/same-trace-states? (trace/trace "a" 29 "b" 31)
;;                                   (trace/trace "a" 29 "b" 31)))
;;     (is (not (trace/same-trace-states? (trace/trace "a" 29 "b" 31)
;;                                        (trace/trace "a" 29 "b" 31 "c" 37))))
;;     (is (not (trace/same-trace-states? (trace/trace "a" 29 "b" 31 "c" 37)
;;                                        (trace/trace "a" 29 "b" 31))))
;;     (is (not (trace/same-trace-states? (trace/trace "a" 29 "b" 31)
;;                                        (trace/trace "a" 29 "b" 31 :value 12))))))

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

;; (deftest delete-1
;;   (testing "delete"
;;     (is (trace/empty-trace? (trace/trace-delete {:value 5})))
;;     (is (trace/same-trace-states? (trace/trace-delete {:value 40 50 {:value 60}}) {50 {:value 60}}))
;;     (is (trace/same-trace-states? (trace/trace-delete {20 {:value 40 50 {:value 60}}} 20) {20 {50 {:value 60}}}))
;;     (is (trace/same-trace-states? (trace/trace-delete {10 {20 {:value 40 50 {:value 60}}}} (list 10 20))
;;                                   {10 {20 {50 {:value 60}}}}))))

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

(deftest merge!-1
  (testing "trace-merge!"
    (let [tr {}]
      (trace/trace-merge tr {5 {:value 55}})
      (is (not (empty? tr)) tr)
      (is (= (count tr) 1) tr)
      (is (= (trace/trace-value tr 5) 55) tr)
      (trace/trace-merge tr {6 {:value 66} 7 {:value 77}})
      (is (= (trace/trace-value tr 7) 77))
      (trace/trace-merge tr {:value 8})
      (is (= (trace/trace-value tr) 8))
      (trace/trace-merge tr {9 {3 {:value 33}}})
      (is (= (trace/trace-value tr '(9 3)) 33)))))

;; (deftest thaw!-1
;;   (testing "thaw1!"
;;     (let [tr (trace/mutable-trace "a" (trace/** (trace/trace "immutable" 7 :value 31))
;;                                   "b" (trace/** (trace/mutable-trace "mutable" 9 :value 33)))]
;;       (is (trace/immutable-trace? (trace/trace-subtrace tr "a")))
;;       (is (trace/mutable-trace? (trace/trace-subtrace tr "b")))
;;       (trace/trace-thaw! tr)

;;       (is (trace/mutable-trace? (trace/trace-subtrace tr "a")))
;;       (is (= (trace/trace-get tr '("a")) 31))
;;       (is (= (trace/trace-get tr '("a" "immutable")) 7))

;;       (is (trace/mutable-trace? (trace/trace-subtrace tr "b")))
;;       (is (= (trace/trace-get tr '("b")) 33))
;;       (is (= (trace/trace-get tr '("b" "mutable")) 9)))))
