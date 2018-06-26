(ns metaprob.infer-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all :as trace]
            [metaprob.sequence :refer [tuple]]
            [metaprob.syntax :refer :all :as syntax]
            [metaprob.builtin-impl :refer :all :as impl]
            [metaprob.builtin :as builtin]
            [metaprob.infer :refer :all :exclude [map replicate apply] :as infer]))

(def top (impl/make-top-level-env 'metaprob.infer))

(deftest frame-1
  (testing "frame smoke test"
    (let [f (infer/make-env top)]
      (infer/env-bind! f "foo" 17)
      (is (= (infer/env-lookup f "foo") 17))
      (is (= (infer/env-lookup f "sub") builtin/sub)))))

(deftest frame-2
  (testing "match-bind! smoke test"
    (let [f (infer/make-env top)
          pat (from-clojure-pattern '[a b])]
      (infer/match-bind! pat (list 1 2) f)
      (is (= (infer/env-lookup f "a") 1))
      (is (= (infer/env-lookup f "b") 2)))))

(defn mk_nil [] (trace/empty-trace))

(defn ez-call [prob-prog & inputs]
  (let [inputs (if (= inputs nil) '() inputs)
        [value score]
        (builtin/sequence-to-seq
         (infer/infer-apply prob-prog
                      inputs
                      (mk_nil) (mk_nil) (mk_nil)))]
    value))

(deftest apply-1
  (testing "Apply a procedure to no inputs"
    (is (trace/empty-trace?
         (ez-call trace/empty-trace)))))

(deftest apply-2
  (testing "Apply a procedure to one arg"
    (is (= (ez-call builtin/sub 7)
           -7))))

(def no-trace (trace))    ;formerly nil

(defn ez-eval [x]
  (let [[value score]
        (infer/infer-eval (from-clojure x) top no-trace no-trace false)]
    value))

(deftest literal-1
  (testing "Interpret a literal expression"
    (is (= (ez-eval '3)
           3))))

(deftest variable-1
  (testing "Interpret a variable"
    (is (= (ez-eval 'trace-get)
           builtin/trace-get))))

(deftest application-0
  (testing "Interpret a call, no args"
    (is (= (ez-eval '(+)) 0))))

(deftest application-1
  (testing "Interpret a call, one arg"
    (is (= (ez-eval '(+ 7)) 7))))

(deftest application-2
  (testing "Interpret a call, two args"
    (is (= (ez-eval '(+ 2 3)) 5))))

(deftest if-1
  (testing "Interpret an if"
    (is (= (ez-eval '(if true 2 3)) 2))))

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

;; Export a procedure i.e. use 'foreign' (clojure) version rather than
;; trying to compile the 'native' version (source code)

(deftest export-1
  (testing "export a procedure"
    (let [x 5
          m1 (gen [] x)
          m2 (infer/opaque "opaque-test" m1)]
      (is (= (m2) (m1))))))

;; Lift a generate method up to a infer method

(deftest lift-1
  (testing "lift a generate method up to a infer method"
    (let [m (gen [inputs i t o]
                      (define [x y] inputs)
                      (builtin/tuple (+ x 1) 19))
          l (infer/inf "testing" m)]
      (is (= (l 17 "z") 18)))))


(deftest lift-and-call
  (testing "can we lift a procedure and then call it"
    (let [qq (impl/make-foreign-procedure "qq" (fn [inputs i t o]
                                             [(+ (builtin/nth inputs 0) (builtin/nth inputs 1))
                                              50]))
          lifted (infer/inf "lifted" qq)]
      (is (= (lifted 7 8) 15))
      (let [[answer score] (infer/infer-apply lifted [7 8] nil nil nil)]
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
    (let [form (from-clojure '(block 17 (sub 19)))
          [value1 output _] (infer/infer-eval form top no-trace no-trace true)]
      (is (= value1 -19))
      (is (= (trace-count output) 1))
      (is (= (trace-get output 1) -19))
      (let [intervene (trace 1 23)]
        (let [[value2 output _] (infer/infer-eval form top intervene no-trace true)]
          (is (= (trace-count output) 1))
          (is (= (trace-get output 1) 23))
          (is (= value2 23)))))))


(deftest intervene-2
  (testing "output capture, then intervention"
    (let [form (from-clojure '(block (add 15 2) (sub 21 2)))
          [value1 output _] (infer/infer-eval form top no-trace no-trace true)]
      (is (= value1 19))
      (let [intervene (builtin/empty-trace)
            addresses (builtin/addresses-of output)]
        ;; (builtin/pprint output)
        (doseq [a addresses]
          (builtin/trace-set! intervene a 23))
        (let [[value2 _] (infer/infer-eval form top intervene no-trace false)]
          (is (= value2 23)))))))

(deftest apply-1
  (testing "apply smoke test"
    (is (= (apply builtin/sub [3 2]) 1))
    (is (= (apply builtin/sub (list 3 2)) 1))
    (is (= (apply apply (list builtin/sub (list 3 2))) 1))))

;; ------------------------------------------------------------------

(def this-map infer/map)

(deftest map-1
  (testing "map smoke test"
    (is (builtin/nth (this-map (gen [x] (builtin/add x 1))
                                  (builtin/list 4 5 6))
                     1)
        6)
    ;; These tests have to run after the call to map
    (is (= (ns-resolve 'metaprob.prelude 'val) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'metaprob.prelude) 'val))
        "namespacing sanity check 2")))

;; I'm sort of tired of this and don't anticipate problems, so
;; not putting more work into tests at this time.


(deftest map-1a
  (testing "Map over a clojure list"
    (let [start (builtin/list 6 7 8)
          foo (this-map (fn [x] (+ x 1))
                        start)]
      (is (builtin/length foo) 3)
      (is (= (builtin/nth foo 0) 7))
      (is (= (builtin/nth foo 1) 8))
      (is (= (builtin/nth foo 2) 9)))))

(deftest map-2
  (testing "Map over a metaprob list"
    (is (= (builtin/first
            (builtin/rest
             (this-map (fn [x] (+ x 1))
                       (builtin/pair 6 (builtin/pair 7 (builtin/pair 8 (builtin/empty-trace)))))))
           8))))

(deftest map-3
  (testing "Map over a metaprob tuple"
    (is (= (builtin/trace-get (this-map (fn [x] (+ x 1))
                                        (builtin/tuple 6 7 8))
                              1)
           8))))


;; Self-application

;; These have to be defined at top level because only top level defined
;; gens can be interpreted (due to inability to understand environments).

(define apply-test
  (gen [thunk]
    (define [val output score]
      (infer-apply-neuf thunk (tuple) (trace) (trace) true))
    output))

(define tst1 (gen [] (builtin/add 2 (builtin/mul 3 5))))
(define tst2 (gen [] (apply-test tst1)))

(deftest infer-apply-self-application
  (testing "apply infer-apply to program that calls infer-apply"

    ;; 3
    (is (> (count (addresses-of (apply-test tst1))) 2))

    ;; 271
    (is (> (count (addresses-of (apply-test tst2))) 100))))
