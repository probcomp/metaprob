(ns metaprob.metacirc.trace-choices-test
  (:require [clojure.test :refer :all]
            [metaprob.environment :as environment]
            [metaprob.trace :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.builtin :as b]
            [metaprob.metacirc.trace-choices :refer :all]))

(defn mk_nil [] (b/mk_nil))

(defn ez-apply [prob-prog & args]
  (trace_choices prob-prog
                 (b/seq-to-metaprob-tuple args)
                 (mk_nil)
                 (mk_nil)))

(deftest apply-1
  (testing "Apply a probprog to no args"
    (is (b/empty-trace?
         (ez-apply b/mk_nil)))))

(deftest apply-2
  (testing "Apply a probprog to one arg"
    (is (= (ez-apply b/sub 7)
           -7))))

(defn ez-eval [x]
  (let [ns 'metaprob.metacirc.trace-choices]
    (binding [*ns* (find-ns ns)]
      (tc_eval (from-clojure x)
               (environment/make-top-level-env ns)
               (mk_nil)
               (mk_nil)))))

(deftest smoke-1
  (testing "Interpret a literal expression"
    (is (= (ez-eval 3)
           3))))

(deftest smoke-2
  (testing "Interpret a variable"
    (is (= (ez-eval 'first)
           b/first))))

;; N.b. this will reify the program to get stuff to eval

(deftest binding-1
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-apply (program [x] x) 5)
           5))))

(deftest binding-2
  (testing "Bind a variable locally to a value (eval)"
    (is (= (ez-eval '((program [x] x) 5))
           5))))

;; map

(deftest map-0
  (testing "map smoke test - empty list"
    (is (= (ez-eval '(length (map (program [x] x) (list))))
           0))))

(deftest map-5
  (testing "map smoke test - debugging 1"
    (let [result (ez-eval '((program []
                               (define root "this")
                               (define f (program [x] x))
                               (define l (list 3))
                               (define
                                 _map
                                 (program
                                  [f l i root]
                                  (block
                                   l)))
                               (_map f l 0 root))))]
      ;; (print "Result:\n") (b/pprint result)
      (is (b/is_pair result))
      (is (= (b/first result) 3)))))

(deftest map-6
  (testing "map smoke test - debugging 2"
    (is (ez-eval '(is_pair (list 3))))
    (let [result (ez-eval '((program []
                               (define root this)
                               (define f (program [x] x))
                               (define l (list 3))
                               (define
                                 _map
                                 (program
                                  [f l i root]
                                  (block
                                   ;; (print "Input 1:") (pprint f)
                                   ;; (print "Input 2:") (pprint l)
                                   (if (is_pair l)
                                     (block
                                      (define val (with-address (list root i) (f (first l))))
                                      (pair val (_map f (rest l) (add i 1) root)))
                                     (mk_nil)))))
                               (_map f l 0 root))))]
      ;; (print "Result:\n") (b/pprint result)
      (is (b/is_pair result))
      (is (= (b/first result) 3)))))


(deftest map-7
  (testing "map smoke test"
    (if false
    (is (= (ez-eval '(first (map (program [x] x) (list 3))))
           3)))))

;; 

(deftest junk-1
  (testing "foo"
    (if false
      (let [flip_coins (ez-eval '(program [n]
                                          (define r (range n))
                                          (pprint r)
                                          (map (program [tag] (flip))
                                               r)))]
        (let [output (b/mk_nil)
              choices (trace_choices flip_coins (tuple 5) (b/mk_nil) output)]
          (print "Output trace:")
          (b/pprint output)
          (print "Result:")
          (b/pprint choices)
          true)))))

        
