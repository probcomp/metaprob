(ns metaprob.compositional-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all :as trace]
            [metaprob.syntax :refer :all :as syntax]
            [metaprob.builtin-impl :refer :all :as impl :exclude [infer-apply]]
            [metaprob.builtin :as builtin]
            [metaprob.distributions :refer :all :as distributions]
            [metaprob.compositional :refer :all :exclude [map replicate apply] :as comp])
  (:refer-clojure :exclude [assoc dissoc]))

(def top (impl/make-top-level-env 'metaprob.compositional))

(def no-trace {})

(defn ez-call [prob-prog & inputs]
  (let [inputs (if (= inputs nil) '() inputs)
        [value _ score]
        (comp/infer-apply prob-prog
                          inputs
                          {:interpretation-id (clojure.core/gensym)
                           :intervene no-trace
                           :target no-trace
                           :active? false})]
    value))

(deftest apply-1
  (testing "Apply a procedure to no inputs"
    (is (empty?
         (ez-call {})))))

(deftest apply-2
  (testing "Apply a procedure to one arg"
    (is (= (ez-call - 7)
           -7))))

(defn ez-eval [x]
  (let [[value output score]
        (comp/infer-eval x
                         top
                         {:interpretation-id (clojure.core/gensym)
                          :intervene no-trace
                          :target no-trace
                          :active? false})]
    value))

(deftest smoke-1
  (testing "Interpret a literal expression"
    (is (= (ez-eval '3)
           3))))

(deftest smoke-2
  (testing "Interpret a variable"
    (is (= (ez-eval 'trace-value)
           builtin/trace-value))))

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
    (let [m (gen [inputs context]
              (define [x y] inputs)
              [(+ x 1) 19])
          l (builtin/inf "testing" nil m)]
      (is (= (l 17 "z") 18)))))

(deftest lift-and-call
  (testing "can we lift a procedure and then call it"
    (let [qq (impl/make-foreign-procedure "qq"
                                          (fn [inputs & junk]
                                            [(+ (builtin/nth inputs 0) (builtin/nth inputs 1))
                                             {}
                                             50]))
          lifted (builtin/inf "lifted" nil qq)]
      (is (= (lifted 7 8) 15))
      (let [[answer output score] (comp/infer-apply
                                   lifted [7 8]
                                   no-trace no-trace false)]
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
    (let [form '(block 17 (+ 19))
          [value1 output1 _] (comp/infer-eval form top
                                              {:interpretation-id (clojure.core/gensym)
                                               :intervene no-trace
                                               :target no-trace
                                               :active? true})
          adr 1]
      (is (= value1 19))
      (let [intervene {1 {:value 23}}]
        (let [[value2 output2 _] (comp/infer-eval form top
                                                  {:interpretation-id (clojure.core/gensym)
                                                   :intervene intervene
                                                   :target no-trace
                                                   :active? true})]
          (is (= value2 23)))))))

(deftest intervene-2
  (testing "output capture, then intervention"
    (let [form '(block (+ 15 2) (- 21 2))
          [value1 output _] (comp/infer-eval form top
                                             {:interpretation-id (clojure.core/gensym)
                                              :intervene no-trace
                                              :target no-trace
                                              :active? true})]
      (is (= value1 19))
      (let [intervene (-> {}
                          (builtin/trace-set-value '(0 "+") 23)
                          (builtin/trace-set-value '(1 "-") 23))
            addresses (builtin/addresses-of output)]
        (let [[value2 output2 _] (comp/infer-eval form top
                                                  {:interpretation-id (clojure.core/gensym)
                                                   :intervene intervene
                                                   :target no-trace
                                                   :active? true})]
          (is (= value2 23)))))))

(comment
  (deftest intervene-3
    (testing "intervention value is not recorded when it is not usually in output trace"
      (let [form '(block (define x (+ 15 2)) (- x 2))
            intervene (trace-set-value {} '(0 "x" "+") 23)
            [value out _] (comp/infer-eval form top
                                           {:interpretation-id (clojure.core/gensym)
                                            :intervene intervene
                                            :target no-trace
                                            :active? true})]
        (is (= value 21))
        (is (empty? out)))))


  (define tst1 (gen []
                 (define x (if (distributions/flip 0.5) 0 1))
                 (+ x 3)))

  (deftest intervene-4
    (testing "intervention value is recorded when it overwrites normally-traced execution"
      (let [intervene (trace-set-value {} '(0 "x") 5)
            [value out __prefix__] (comp/infer-apply
                                    tst1 []
                                    {:interpretation-id (clojure.core/gensym)
                                     :intervene intervene
                                     :target no-trace
                                     :active? true})]
        ;; (clojure.core/println "value" value)
        (clojure.core/println "out" out
                              "value?" (trace-has-value? out '(0 "x")))
        (is (= value 8))
        (is (and (trace-has-value? out '(0 "x"))
                 (= 5 (trace-value out '(0 "x"))))))))

  (deftest intervene-target-agree
    (testing "intervention and target traces agree"
      (let [intervene (trace-set-value {} '(0 "x") 5)
            target (trace-set-value {} '(0 "x") 5)
            [value out s] (comp/infer-apply tst1 []
                                            {:interpretation-id (clojure.core/gensym)
                                             :intervene intervene
                                             :target target
                                             :active? true})]
        (println "out" out)
        (is (= value 8))
        (is (= s 0))
        (is (and (trace-has-value? out '(0 "x"))
                 (= 5 (trace-value out '(0 "x"))))))))

  ;; jmt an assert keeps this from happening. if that's expected, this
  ;; test should change to catch that AssertionError
  (comment
    (deftest intervene-target-disagree
      (testing "intervention and target traces disagree"
        (let [intervene (trace-set-value {} '(0 "x") 6)
              target (trace-set-value {} '(0 "x") 5)
              [value out s] (comp/infer-apply tst1 []
                                              {:interpretation-id (clojure.core/gensym)
                                               :intervene intervene
                                               :target target
                                               :active? true})]
          (is (= value 8))
          (is (= s builtin/negative-infinity))
          (is (and (trace-has-value? out '(0 "x"))
                   (= 5 (trace-value out '(0 "x")))))))))

  ;; jmt an assert keeps this from happening. if that's expected, this
  ;; test should change to catch that AssertionError
  (comment
    (deftest impossible-target
      (testing "target is impossible value"
        (let [target (trace-set-value {} '(0 "x") 5)
              [value out s] (comp/infer-apply tst1 []
                                              {:interpretation-id (clojure.core/gensym)
                                               :intervene {}
                                               :target target
                                               :active? true})]
          (is (= value 8))
          (is (= s builtin/negative-infinity))
          (is (and (trace-has-value? out '(0 "x"))
                   (= 5 (trace-value out '(0 "x")))))))))

  (deftest true-target
    (testing "target value is the true value"
      (let [form '(block (define x (+ 15 2)) (- x 2))
            target (trace-set-value {} '(0 "x" "+") 17)
            [value out s] (comp/infer-eval form top {:interpretation-id (clojure.core/gensym)
                                                     :intervene no-trace
                                                     :target no-trace
                                                     :active? true})]
        (is (= value 15))
        (is (= s 0))
        (is (empty? out)))))

  ;; Self-application

  ;; These have to be defined at top level because only top level
  ;; defined gens can be interpreted (due to inability to understand
  ;; environments).

  ;; TODO: Explore what it would take to remove this limitation.
  (define apply-test
    (gen [thunk]
      (define [val output score]
        (infer-apply thunk [] {:interpretation-id (clojure.core/gensym)
                               :intervene no-trace
                               :target no-trace
                               :active? true}))
      output))

  (define tst2 (gen [] (distributions/flip 0.5)))
  (define tst3 (gen [] (apply-test tst2)))

  (deftest infer-apply-self-application
    (testing "apply infer-apply to program that calls infer-apply"
      (binding [*ambient-interpreter* infer-apply]
        ;; When we interpret tst1 directly, the value of flip is
        ;; recorded at the length-1 address '(distributions/flip).
        (is (= (count (first (addresses-of (apply-test tst2)))) 1))

        ;; But when we trace the execution of the interpreter, the
        ;; address at which the random choice is recorded is
        ;; significantly longer, due to the complex chain of function
        ;; calls initiated by the interpreter.
        (is (> (count (first (addresses-of (apply-test tst3)))) 10))))))
