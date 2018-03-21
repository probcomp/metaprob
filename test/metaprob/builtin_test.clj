(ns metaprob.builtin-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.builtin :as b]
            [metaprob.builtin-impl :as impl]))

(deftest last-1
  (testing "Last element of a metaprob list"
    (is (= (b/last (impl/seq-to-metaprob-list '(1 2 3)))
           3))))

(deftest append-1
  (testing "Concatenate two metaprob lists"
    (let [l1 (impl/seq-to-metaprob-list '(1 2 3))
          l2 (impl/seq-to-metaprob-list '(7 8 9))]
      (is (= (b/last (b/append l1 l2))
             9)))))

(deftest list-1
  (testing "Assemble and access a mp-list"
    (is (= (b/first (b/list 4 5 6))
           4))))

(deftest list-2
  (testing "Assemble and access a mp-list"
    (is (= (b/first (b/rest (b/list 4 5 6)))
           5))))

(deftest array2list
  (testing "Convert metaprob tuple to metaprob list"
    (let [v [5 7 11 13]
          t (impl/seq-to-metaprob-tuple v)]
      (is (impl/metaprob-tuple? t))
      (let [l (b/array-to-list t)]
        (is (impl/metaprob-pair? l))
        (let [v2 (vec (impl/metaprob-list-to-seq l))]
          (is (= v2 v)))))))

(deftest list2array
  (testing "Convert metaprob list to metaprob tuple"
    (let [v [5 7 11 13]
          t (impl/seq-to-metaprob-list v)]
      (is (impl/metaprob-pair? t))
      (let [l (b/list-to-array t)]
        (is (impl/metaprob-tuple? l))
        (let [v2 (vec (impl/metaprob-tuple-to-seq l))]
          (is (= v2 v)))))))

(deftest tag-capture
  (testing "capture- and retrieve-tag-address smoke test"
    (let [root (new-trace "root")
          q (b/capture-tag-address root root root)
          a (b/list "this" "that")
          r (b/resolve-tag-address (b/pair q a))
          o2 (b/nth r 2)]
      (is (trace? o2))
      (b/trace-set o2 "value")
      (is (= (b/trace-get o2) "value"))
      (is (= (b/trace-get (b/lookup root a)) "value"))
      (is (= (b/trace-get root a) "value")))))

;; Probprog stuff

(deftest foreign-probprog
  (testing "create and call a foreign-probprog"
    (let [pp (impl/make-foreign-probprog "pp" (fn [x] (+ x 1)))]
      (is (= (b/generate-foreign pp [6]) 7)))))

(deftest basic-query
  (testing "query a foreign-probprog"
    (let [pp (impl/make-foreign-probprog "pp" (fn [x] (+ x 1)))]
      (let [[answer score] (impl/mini-query pp [7] nil nil nil)]
        (is (= score 0))
        (is (= answer 8))))))

(deftest lift-and-query
  (testing "can we lift a probprog and then query it"
    (let [qq (impl/make-foreign-probprog "qq" (fn [argseq i t o]
                                             [(+ (b/nth argseq 0) (b/nth argseq 1))
                                              50]))
          lifted (b/make-lifted-probprog "lifted" qq)]
      (let [[answer score] (impl/mini-query lifted [7 8] nil nil nil)]
        (is (= answer 15))
        (is (= score 50))))))

(deftest reification-1
  (testing "Does a probprog appear to be a trace?"
    (let [pp (impl/make-foreign-probprog "pp" (fn [x] (+ x 1)))]
      (is (= (b/trace-get pp) "prob prog")))))

(deftest length-1
  (testing "length smoke test"
    (is (= (b/length (b/empty-trace)) 0))
    (is (= (b/length (b/pair 0 (b/empty-trace))) 1))
    (is (= (b/length (impl/seq-to-metaprob-list [1 2 3 4])) 4))))

(deftest range-1
  (testing "range smoke test"
    (let [r (b/range 5)]
      (is (= (b/length r) 5))
      (is (= (count (impl/metaprob-list-to-seq r)) 5))
      (is (= (b/first r) 0))
      (is (= (b/last r) 4)))))

;; addresses-of

(deftest addresses-of-1
  (testing "Smoke test addresses-of"
    (let [tree (trace-from-map {"x" (trace-from-map {"a" (new-trace 1)
                                                   "b" (new-trace 2)
                                                   "c" (new-trace)})
                               "y" (new-trace "d")})
          sites (b/addresses-of tree)]
      (is (= (b/length sites) 3)))))

;; match-bind

;; (deftest match-bind-1
;;   (testing "match-bind smoke"
;;     (let [env (make-env ... what a pain in the ass ...)]
;;       (b/match-bind (from-clojure '[a b])
;;                     [1 2]
;;                     env)
;;       (is (= (env-lookup env "a") 1))
;;       (is (= (env-lookup env "b") 2)))))


(deftest list-contains-1
  (testing "smoke test metaprob-list-contains"
    (is (impl/metaprob-list-contains? (impl/seq-to-metaprob-list '(3 5 7))
                                   5))))

(deftest list-contains-2
  (testing "smoke test metaprob-list-contains"
    (is (not (impl/metaprob-list-contains? (impl/seq-to-metaprob-list '(3 5 7))
                                        11)))))

(deftest set-difference-1
  (testing "smoke test set-difference"
    (let [a (impl/seq-to-metaprob-list '(3 5 7))
          b (impl/seq-to-metaprob-list '(5 7 11 13))]
      (is (impl/metaprob-list-contains? a 5) "5 in a")
      (let [a-b (b/set-difference a b)
            b-a (b/set-difference b a)]
        (is (impl/metaprob-list-contains? a-b 3) "3 in a-b")
        (is (impl/metaprob-list-contains? b-a 13) "13 in b-a")
        (is (not (impl/metaprob-list-contains? a-b 7)) "7 not in a-b")
        (is (not (impl/metaprob-list-contains? b-a 7)) "7 not in b-a")))))

(deftest hairy-key-1
  (testing "does it work to use a probprog name as a trace"
    (let [pp (program [x] x)
          pt (impl/tracify pp)
          key (value (subtrace pt "name"))
          tr (trace-from-map {key (new-trace 17)})]
      (is (= (value (subtrace tr key)) 17)))))

(deftest addresses-of-1
  (testing "addresses-of (addresses-of)"
    (let [tr (trace-from-map {"a" (new-trace 17)
                              "b" (new-trace 31)
                              "c" (trace-from-map {"d" (new-trace 71)})})
          sites (impl/metaprob-collection-to-seq (b/addresses-of tr))
          vals  (map (fn [site] (value (b/lookup tr site))) sites)
          has? (fn [val] (some (fn [x] (= x val)) vals))]
      (has? 17)
      (has? 71))))
