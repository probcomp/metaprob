(ns metaprob.compositional-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all :as trace]
            [metaprob.sequence :refer [tuple]]
            [metaprob.syntax :refer :all :as syntax]
            [metaprob.builtin-impl :refer :all :as impl :exclude [infer-apply]]
            [metaprob.builtin :as builtin]
            [metaprob.compositional :refer :all :exclude [map replicate apply] :as comp]))

(def top (impl/make-top-level-env 'metaprob.compositional))

(def no-trace (trace/trace))

(defn ez-call [prob-prog & inputs]
  (let [inputs (if (= inputs nil) '() inputs)
        [value _ score]
        (comp/infer-apply prob-prog
                           inputs
                           no-trace no-trace false)]
    value))

(deftest apply-1
  (testing "Apply a procedure to no inputs"
    (is (trace/empty-trace?
         (ez-call trace/empty-trace)))))

(deftest apply-2
  (testing "Apply a procedure to one arg"
    (is (= (ez-call builtin/sub 7)
           -7))))

(defn ez-eval [x]
  (let [[value output score]
        (comp/infer-eval (from-clojure x)
                          top
                          no-trace
                          no-trace
                          false)]
    value))

(deftest smoke-1
  (testing "Interpret a literal expression"
    (is (= (ez-eval '3)
           3))))

(deftest smoke-2
  (testing "Interpret a variable"
    (is (= (ez-eval 'trace-get)
           builtin/trace-get))))

(deftest thunk-1
  (testing "call a thunk"
    (is (= (ez-call (ez-eval '(gen [] 7)))
           7))))

;; N.b. this will reify the procedure to get stuff to eval

(deftest binding-1
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-call (gen [x] x) 5)
           5))))

(deftest binding-2
  (testing "Bind a variable locally to a value (eval)"
    (is (= (ez-eval '((gen [x] x) 5))
           5))))

(deftest binding-3
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-call (ez-eval '(gen [] (define x 17) x)))
           17))))

(deftest n-ary-1
  (testing "n-ary procedure, formal parameter list is [& y]"
    (is (= (first (ez-call (ez-eval '(gen [& y] y))
                           8 9))
           8))))

(deftest n-ary-2
  (testing "n-ary procedure, & in formal parameter list"
    (let [result (ez-call (ez-eval '(gen [x & y] y))
                          7 8 9 10)]
      (is (seq? result))
      (is (= (count result) 3))
      (is (= (first result) 8)))))

;; Lift a generate method up to a infer method
;; TBD: Set *ambient-interpreter*!

(deftest lift-1
  (testing "lift a generate method up to a infer method"
    (let [m (gen [inputs i t o]
                      (define [x y] inputs)
                      (builtin/tuple (+ x 1) 19))
          l (builtin/inf "testing" nil m)]
      (is (= (l 17 "z") 18)))))


(deftest lift-and-call
  (testing "can we lift a procedure and then call it"
    (let [qq (impl/make-foreign-procedure "qq"
                                          (fn [inputs & junk]
                                            [(+ (builtin/nth inputs 0) (builtin/nth inputs 1))
                                             (trace)
                                             50]))
          lifted (builtin/inf "lifted" nil qq)]
      (is (= (lifted 7 8) 15))
      (let [[answer output score] (comp/infer-apply lifted [7 8] no-trace no-trace false)]
        (is (= answer 15))
        (is (= score 50))))))

(deftest and-1
  (testing "and smoke test"
    (is (= (ez-eval '(and)) true))
    (is (= (ez-eval '(and 1)) 1))
    (is (= (ez-eval '(and 1 2)) 2))
    (is (= (ez-eval '(and 1 false)) false))
    (is (= (ez-eval '(and 1 2 3)) 3))))

(deftest or-1
  (testing "or smoke test"
    (is (= (ez-eval '(or)) false))
    (is (= (ez-eval '(or 1)) 1))
    (is (= (ez-eval '(or 1 2)) 1))
    (is (= (ez-eval '(or false 2)) 2))
    (is (= (ez-eval '(or false false 3)) 3))))

(deftest case-1
  (testing "case smoke test"
    (is (= (ez-eval '(case 1 2)) 2))
    (is (= (ez-eval '(case 1 1 2)) 2))
    (is (= (ez-eval '(case 1 1 2 3)) 2))
    (is (= (ez-eval '(case 1 2 3 1 4)) 4))))


(deftest intervene-1
  (testing "simple intervention"
    (let [form (from-clojure '(block 17 (+ 19)))
          [value1 output1 _] (comp/infer-eval form top no-trace no-trace true)
          adr 1]
      (is (= value1 19))
      (let [intervene (trace-set (builtin/trace) 1 23)]
        (let [[value2 output2 _] (comp/infer-eval form top intervene no-trace true)]
          (is (= value2 23)))))))


(deftest intervene-2
  (testing "output capture, then intervention"
    (let [form (from-clojure '(block (add 15 2) (sub 21 2)))
          [value1 output _] (comp/infer-eval form top no-trace no-trace true)]
      (is (= value1 19))
      (let [intervene (builtin/empty-trace)
            addresses (builtin/addresses-of output)]
        ;; (builtin/pprint output)
        (doseq [a addresses]
          (builtin/trace-set! intervene a 23))
        (let [[value2 output2 _] (comp/infer-eval form top intervene no-trace true)]
          (is (= value2 23)))))))

;; Self-application

;; These have to be defined at top level because only top level defined
;; gens can be interpreted (due to inability to understand environments).

(define apply-test
  (gen [thunk]
    (define [val output score]
      (infer-apply thunk [] no-trace no-trace true))
    output))

(define tst1 (gen [] (builtin/add 2 (builtin/mul 3 5))))
(define tst2 (gen [] (apply-test tst1)))

(deftest infer-apply-self-application
  (testing "apply infer-apply to program that calls infer-apply"

    ;; 2
    (is (> (count (addresses-of (apply-test tst1))) 1))

    ;; 271
    (is (> (count (addresses-of (apply-test tst2))) 100))))
