(ns metaprob.compositional-test
  (:require [clojure.test :refer [deftest is testing]]
            [metaprob.trace :refer :all :as trace]
            [metaprob.builtin :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.distributions :refer :all :as distributions]
            [metaprob.prelude :refer :all])
  (:refer-clojure :exclude [assoc dissoc]))

(defn ez-call [prob-prog & inputs]
  (let [inputs (if (= inputs nil) '() inputs)
        [value _ _]
        (infer-and-score :procedure prob-prog :inputs inputs)]
    value))

(deftest apply-2
  (testing "Apply a procedure to one arg"
    (is (= (ez-call - 7)
           -7))))

(deftest thunk-1
  (testing "call a thunk"
    (is (= (ez-call (gen [] 7))
           7))))

;; N.b. this will reify the procedure to get stuff to eval

(deftest binding-1
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-call (gen [x] x) 5)
           5))))

(deftest binding-3
  (testing "Bind a variable locally to a value (apply)"
    (is (= (ez-call (gen [] (let [x 17] x)))
           17))))

(deftest n-ary-1
  (testing "n-ary procedure, formal parameter list is [& y]"
    (is (= (first (ez-call (gen [& y] y)
                           8 9))
           8))))

(deftest n-ary-2
  (testing "n-ary procedure, & in formal parameter list"
    (let [result (ez-call  (gen [x & y] y)
                          7 8 9 10)]
      (is (seq? result))
      (is (= (count result) 3))
      (is (= (first result) 8)))))

(deftest lift-1
  (testing "lift a generate method up to a infer method"
    (let [m (gen [inputs observation-trace intervention-trace]
              (let [[x y] inputs]
                [(+ x 1) {} 19]))
          l (inf nil m)]
      (is (= (l 17 "z") 18)))))

(deftest lift-and-call
  (testing "can we lift a procedure and then call it"
    (let [qq
          (fn [inputs & junk]
            [(+ (nth inputs 0) (nth inputs 1))
             {}
             50])
          lifted (inf :no-model qq)]
      (is (= (lifted 7 8) 15))
      (let [[answer output score] (infer-and-score
                                   :procedure lifted :inputs [7 8])]
        (is (= answer 15))
        (is (= score 50))))))
;
;;; `case` expands to use clojure-internal `case*`, which can't work in
;;; metaprob until we implement that (or manually expand `case`). Until
;;; we do that, I'm going to leave this commented-out and file an
;;; issue (jmt)
;(comment (deftest case-1
;          (testing "case smoke test"
;            (is (= (ez-eval (mp-expand '(case 1 2))) 2))
;            (is (= (ez-eval (mp-expand '(case 1 1 2))) 2))
;            (is (= (ez-eval (mp-expand '(case 1 1 2 3))) 2))
;            (is (= (ez-eval (mp-expand '(case 1 2 3 1 4))) 4)))))
;
;(deftest intervene-1
;  (testing "simple intervention"
;    (let [form '(block 17 (+ 19))
;          [value1 output1 _] (comp/infer-eval form top
;                                              {:interpretation-id (clojure.core/gensym)
;                                               :intervene no-trace
;                                               :target no-trace
;                                               :active? true})
;          adr 1]
;      (is (= value1 19))
;      (let [intervene {1 {:value 23}}]
;        (let [[value2 output2 _] (comp/infer-eval form top
;                                                  {:interpretation-id (clojure.core/gensym)
;                                                   :intervene intervene
;                                                   :target no-trace
;                                                   :active? true})]
;          (is (= value2 23)))))))
;
;(deftest intervene-2
;  (testing "output capture, then intervention"
;    (let [form '(block (+ 15 2) (- 21 2))
;          [value1 output _] (comp/infer-eval form top
;                                             {:interpretation-id (clojure.core/gensym)
;                                              :intervene no-trace
;                                              :target no-trace
;                                              :active? true})]
;      (is (= value1 19))
;      (let [intervene (-> {}
;                          (builtin/trace-set-value '(0 "+") 23)
;                          (builtin/trace-set-value '(1 "-") 23))
;            addresses (builtin/addresses-of output)]
;        (let [[value2 output2 _] (comp/infer-eval form top
;                                                  {:interpretation-id (clojure.core/gensym)
;                                                   :intervene intervene
;                                                   :target no-trace
;                                                   :active? true})]
;          (is (= value2 23)))))))
;
;(deftest intervene-3
;    (testing "intervention value is not recorded when it is not usually in output trace"
;      (let [form '(block (define x (+ 15 2)) (- x 2))
;            intervene (trace-set-value {} '(0 "x" "+") 23)
;            [value out _] (comp/infer-eval form top
;                                           {:interpretation-id (clojure.core/gensym)
;                                            :intervene intervene
;                                            :target no-trace
;                                            :active? true})]
;        (is (= value 21))
;        (is (empty? out)))))
;
;
;(define tst1
;  (gen []
;    (define x (if (distributions/flip 0.5) 0 1))
;    (+ x 3)))
;
;(deftest intervene-4
;  (testing "intervention value is recorded when it overwrites normally-traced execution"
;    (let [intervene (trace-set-value {} '(0 "x") 5)
;          [value out __prefix__] (comp/infer-apply
;                                  tst1 []
;                                  {:interpretation-id (clojure.core/gensym)
;                                   :intervene intervene
;                                   :target no-trace
;                                   :active? true})]
;      (is (= value 8)))))
;
;;; in situations where an intervention targets a non-random site,
;;; `infer`'s _value_ should be affected, but the returned trace should
;;; still contain the (unused) random choices
;(deftest intervene-target-disagree
;  (testing "intervention and target traces disagree"
;    (let [intervene (trace-set-value {} '(0 "x") 5)
;          target (trace-set-value {} '(0 "x") 5)
;          [value out s] (comp/infer-apply tst1 []
;                                          {:interpretation-id (clojure.core/gensym)
;                                           :intervene intervene
;                                           :target target
;                                           :active? true})]
;      (is (= value 8))
;      (is (= s 0))
;      (trace-has-value? out '(0 "x" "predicate" "distributions/flip"))
;      (is (and (trace-has-value? out '(0 "x" "predicate" "distributions/flip"))
;               (clojure.core/contains?
;                #{true false}
;                (trace-value out '(0 "x" "predicate" "distributions/flip"))))))))
;
;
;;; an assert keeps this from working. if that's expected, this test
;;; should change to catch that AssertionError (jmt)
;(deftest intervene-target-disagree
;  (testing "intervention and target traces disagree. this should throw."
;    (let [intervene (trace-set-value {} '(0 "x") 6)
;          target (trace-set-value {} '(0 "x") 5)]
;
;      (is (thrown? AssertionError
;                   (comp/infer-apply tst1 []
;                                     {:interpretation-id (clojure.core/gensym)
;                                      :intervene intervene
;                                      :target target
;                                      :active? true}))))))
;
;
;;; an assert keeps this from happening. if that's expected, this test
;;; should change to catch that AssertionError (jmt)
;(deftest impossible-target
;  (testing "target is impossible value"
;    (let [target (trace-set-value {} '(0 "x") 5)]
;
;      (is (thrown? AssertionError
;                   (comp/infer-apply tst1 []
;                                     {:interpretation-id (clojure.core/gensym)
;                                      :intervene {}
;                                      :target target
;                                      :active? true}))))))
;
;(deftest true-target
;  (testing "target value is the true value"
;    (let [form '(block (define x (+ 15 2)) (- x 2))
;          target (trace-set-value {} '(0 "x" "+") 17)
;          [value out s] (comp/infer-eval form top
;                                         {:interpretation-id (clojure.core/gensym)
;                                          :intervene no-trace
;                                          :target no-trace
;                                          :active? true})]
;      (is (= value 15))
;      (is (= s 0))
;      (is (empty? out)))))
;
;;; Self-application
;
;;; These have to be defined at top level because only top level
;;; defined gens can be interpreted (due to inability to understand
;;; environments).
;
;;; TODO: Explore what it would take to remove this limitation.
;(define apply-test
;  (gen [thunk]
;    (define [val output score]
;      (infer-apply thunk [] {:interpretation-id (clojure.core/gensym)
;                             :intervene no-trace
;                             :target no-trace
;                             :active? true}))
;    output))
;
;(define tst2 (gen [] (distributions/flip 0.5)))
;(define tst3 (gen [] (apply-test tst2)))
;
;(deftest infer-apply-self-application
;  (testing "apply infer-apply to program that calls infer-apply"
;    (binding [*ambient-interpreter* infer-apply]
;      ;; When we interpret tst1 directly, the value of flip is
;      ;; recorded at the length-1 address '(distributions/flip).
;      (is (= (count (first (addresses-of (apply-test tst2)))) 1))
;
;      ;; But when we trace the execution of the interpreter, the
;      ;; address at which the random choice is recorded is
;      ;; significantly longer, due to the complex chain of function
;      ;; calls initiated by the interpreter.
;      (is (> (count (first (addresses-of (apply-test tst3)))) 10)))))
