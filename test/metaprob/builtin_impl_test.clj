(ns metaprob.builtin-impl-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace :refer [trace]]
            [metaprob.builtin-impl :refer :all]))

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

(deftest list-1
  (testing "Assemble and access a mp-list"
    (is (= (trace/metaprob-first (metaprob-list 4 5 6))
           4))))

(deftest list-2
  (testing "Assemble and access a mp-list"
    (is (= (trace/metaprob-first (trace/metaprob-rest (metaprob-list 4 5 6)))
           5))))

(deftest tuple2list
  (testing "Convert metaprob tuple to metaprob list"
    (let [v [5 7 11 13]
          t v]  ;was: (seq-to-mutable-tuple v)
      (is (trace/metaprob-tuple? t))
      (let [l (to-list t)]
        (is (trace/metaprob-pair? l))
        (let [v2 (vec (trace/metaprob-list-to-seq l))]
          (is (= v2 v)))))))

(deftest list2tuple
  (testing "Convert metaprob list to metaprob tuple"
    (let [l '(5 7 11 13)
          t (seq-to-mutable-list l)]
      (is (trace/metaprob-pair? t))
      (is (trace/mutable-trace? t))
      (is (= (trace/length t) 4))
      (let [tup (to-tuple t)]
        (is (trace/metaprob-tuple? tup))
        (is (= (trace/length tup) 4))
        (let [s (trace/metaprob-tuple-to-seq tup)]
          (is (seq? s))
          (is (= (count s) 4))
          (is (= (trace/length s) 4))
          (let [v2 (vec s)]
            (is (= v2 tup))))))))

;; Procedure stuff

(deftest foreign-procedure
  (testing "create and call a foreign-procedure"
    (let [pp (make-foreign-procedure "pp" (fn [x] (+ x 1)))]
      (is (= (generate-foreign pp [6]) 7)))))

(deftest length-2
  (testing "length smoke test"
    (let [m (seq-to-mutable-list [1 2 3 4])]
      (is (= (trace/length m) 4))
      (is (= (trace/length (trace/metaprob-sequence-to-seq m)) 4)))))

(deftest range-1
  (testing "range smoke test"
    (let [r (metaprob-range 5)]
      (is (= (trace/length r) 5))
      (is (= (count (trace/metaprob-list-to-seq r)) 5))
      (is (= (trace/metaprob-first r) 0))
      (is (= (metaprob-last r) 4)))))

;; addresses-of

(deftest addresses-of-1
  (testing "Smoke test addresses-of"
    (let [tree (trace/trace-from-map
                {"x" (trace/trace-from-map {"a" (trace/new-trace 1)
                                            "b" (trace/new-trace 2)
                                            "c" (trace/empty-trace)})
                 "y" (trace/new-trace "d")})
          sites (addresses-of tree)]
      (is (= (trace/length sites) 3)))))

;; match-bind

;; (deftest match-bind-1
;;   (testing "match-bind smoke"
;;     (let [env (make-env ... what a pain in the ass ...)]
;;       (match-bind (from-clojure '[a b])
;;                   [1 2]
;;                   env)
;;       (is (= (env-lookup env "a") 1))
;;       (is (= (env-lookup env "b") 2)))))

;; Does list s contain element x?

(defn metaprob-list-contains? [s x]
  (if (trace/empty-trace? s)
    false
    (if (= x (trace/metaprob-first s))
      true
      (metaprob-list-contains? (trace/metaprob-rest s) x))))


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
          sites (trace/metaprob-sequence-to-seq (addresses-of tr))
          vals  (map (fn [site] (trace/trace-get tr site)) sites)
          has? (fn [val] (some (fn [x] (= x val)) vals))]
      (has? 17)
      (has? 71))))

(deftest nth-1
  (testing "nth smoke tests"
    (is (= (metaprob-nth (list 5 7 11) 1) 7))
    (is (= (metaprob-nth (vector 5 7 11) 1) 7))
    (is (= (metaprob-nth (metaprob-list 5 7 11) 1) 7))
    (is (= (metaprob-nth (metaprob-list 5 7 11) 0) 5))
    (is (= (metaprob-nth (metaprob-list 5 7 11) 2) 11))))

(deftest sample-1
  (testing "sample-uniform smoke tests"
    (let [x (sample-uniform)
          y (sample-uniform)]
      (is (> x 0))
      (is (< x 1))
      (is (> y 0))
      (is (< y 1))
      (is (not (= x y))))))

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


