(ns metaprob.builtin-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.builtin :as b]))

(deftest last-1
  (testing "Last element of a metaprob list"
    (is (= (b/last (b/seq-to-metaprob-list '(1 2 3)))
           3))))

(deftest append-1
  (testing "Concatenate two metaprob lists"
    (let [l1 (b/seq-to-metaprob-list '(1 2 3))
          l2 (b/seq-to-metaprob-list '(7 8 9))]
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
          t (b/seq-to-metaprob-tuple v)]
      (is (b/metaprob-tuple? t))
      (let [l (b/array_to_list t)]
        (is (b/metaprob-pair? l))
        (let [v2 (vec (b/metaprob-list-to-seq l))]
          (is (= v2 v)))))))

(deftest list2array
  (testing "Convert metaprob list to metaprob tuple"
    (let [v [5 7 11 13]
          t (b/seq-to-metaprob-list v)]
      (is (b/metaprob-pair? t))
      (let [l (b/list_to_array t)]
        (is (b/metaprob-tuple? l))
        (let [v2 (vec (b/metaprob-tuple-to-seq l))]
          (is (= v2 v)))))))

(deftest tag_capture
  (testing "capture_ and retrieve_tag_address smoke test"
    (let [root (new-trace "root")
          q (b/capture_tag_address root root root)
          a (b/list "this" "that")
          r (b/resolve_tag_address (b/pair q a))
          o2 (b/nth r 2)]
      (is (trace? o2))
      (b/trace_set o2 "value")
      (is (= (b/trace-get o2) "value"))
      (is (= (b/trace-get (b/lookup root a)) "value"))
      (is (= (b/trace-get root a) "value")))))

;; Probprog stuff

(deftest foreign-probprog
  (testing "create and call a foreign-probprog"
    (let [pp (b/make-foreign-probprog "pp" (fn [x] (+ x 1)))]
      (is (= (b/generate-foreign pp [6]) 7)))))

(deftest basic-query
  (testing "query a foreign-probprog"
    (let [pp (b/make-foreign-probprog "pp" (fn [x] (+ x 1)))]
      (let [[answer score] (b/mini-query pp [7] nil nil nil)]
        (is (= score 0))
        (is (= answer 8))))))

(deftest lift-and-query
  (testing "can we lift a probprog and then query it"
    (let [qq (b/make-foreign-probprog "qq" (fn [argseq i t o]
                                             [(+ (b/nth argseq 0) (b/nth argseq 1))
                                              50]))
          lifted (b/make-lifted-probprog "lifted" qq)]
      (let [[answer score] (b/mini-query lifted [7 8] nil nil nil)]
        (is (= answer 15))
        (is (= score 50))))))

(deftest reification-1
  (testing "Does a probprog appear to be a trace?"
    (let [pp (b/make-foreign-probprog "pp" (fn [x] (+ x 1)))]
      (is (= (b/trace-get pp) "prob prog")))))

(deftest length-1
  (testing "length smoke test"
    (is (= (b/length (b/empty-trace)) 0))
    (is (= (b/length (b/pair 0 (b/empty-trace))) 1))
    (is (= (b/length (b/seq-to-metaprob-list [1 2 3 4])) 4))))

(deftest range-1
  (testing "range smoke test"
    (let [r (b/range 5)]
      (is (= (b/length r) 5))
      (is (= (count (b/metaprob-list-to-seq r)) 5))
      (is (= (b/first r) 0))
      (is (= (b/last r) 4)))))

;; The real `map` is now in the prelude, but keeping the old one
;; because hard to throw away code I worked on!

(deftest map-1
  (testing "Map over a clojure list"
    (let [foo (b/mp-map (fn [x] (+ x 1))
                        (b/list 6 7 8))]
      (is (b/length foo) 3)
      (is (= (b/nth foo 0) 7))
      (is (= (b/nth foo 1) 8))
      (is (= (b/nth foo 2) 9)))))

(deftest map-2
  (testing "Map over a metaprob list"
    (is (= (b/first
            (b/rest
             (b/mp-map (fn [x] (+ x 1))
                       (b/pair 6 (b/pair 7 (b/pair 8 (b/empty-trace)))))))
           8))))

(deftest map-3
  (testing "Map over a metaprob array/tuple"
    (is (= (value-at (b/mp-map (fn [x] (+ x 1))
                               (tuple 6 7 8))
                     '(1))
           8))))

;; Test nondeterministic primitive

(deftest flip-1
  (testing "Try doing a flip"
    (is (boolean? (b/flip)))
    (is (boolean? (b/flip 0.5)))))

(deftest score-1
  (testing "check the score for a flip"
    (let [[answer score] (b/mini-query b/flip [] nil nil nil)]
      (is (number? score))
      (is (> score -2))
      (is (< score 0)))))

;; trace_sites

(deftest trace_sites-1
  (testing "Smoke test trace_sites"
    (let [tree (trace-from-map {"x" (trace-from-map {"a" (new-trace 1)
                                                   "b" (new-trace 2)
                                                   "c" (new-trace)})
                               "y" (new-trace "d")})
          sites (b/trace_sites tree)]
      (is (= (b/length sites) 3)))))

;; match_bind

;; (deftest match_bind-1
;;   (testing "match_bind smoke"
;;     (let [env (make_env ... what a pain in the ass ...)]
;;       (b/match_bind (from_clojure '[a b])
;;                     [1 2]
;;                     env)
;;       (is (= (env_lookup env "a") 1))
;;       (is (= (env_lookup env "b") 2)))))


(deftest list-contains-1
  (testing "smoke test metaprob-list-contains"
    (is (b/metaprob-list-contains? (b/seq-to-metaprob-list '(3 5 7))
                                   5))))

(deftest list-contains-2
  (testing "smoke test metaprob-list-contains"
    (is (not (b/metaprob-list-contains? (b/seq-to-metaprob-list '(3 5 7))
                                        11)))))

(deftest set-difference-1
  (testing "smoke test set-difference"
    (let [a (b/seq-to-metaprob-list '(3 5 7))
          b (b/seq-to-metaprob-list '(5 7 11 13))]
      (is (b/metaprob-list-contains? a 5) "5 in a")
      (let [a-b (b/set-difference a b)
            b-a (b/set-difference b a)]
        (is (b/metaprob-list-contains? a-b 3) "3 in a-b")
        (is (b/metaprob-list-contains? b-a 13) "13 in b-a")
        (is (not (b/metaprob-list-contains? a-b 7)) "7 not in a-b")
        (is (not (b/metaprob-list-contains? b-a 7)) "7 not in b-a")))))

(deftest hairy-key-1
  (testing "does it work to use a probprog name as a trace"
    (let [pp (program [x] x)
          pt (b/tracify pp)
          key (value (subtrace pt "name"))
          tr (trace-from-map {key (new-trace 17)})]
      (is (= (value (subtrace tr key)) 17)))))

(deftest trace_sites-1
  (testing "trace_sites (addresses_of)"
    (let [tr (trace-from-map {"a" (new-trace 17)
                              "b" (new-trace 31)
                              "c" (trace-from-map {"d" (new-trace 71)})})
          sites (b/metaprob-collection-to-seq (b/trace_sites tr))
          vals  (map (fn [site] (value (b/lookup tr site))) sites)
          has? (fn [val] (some (fn [x] (= x val)) vals))]
      (has? 17)
      (has? 71))))
