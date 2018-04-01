(ns metaprob.infer-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace]
            [metaprob.syntax :as syntax :refer :all]
            [metaprob.builtin-impl :as impl]
            [metaprob.builtin :as builtin]
            [metaprob.infer :refer :all :exclude [apply]]))

(deftest frame-1
  (testing "frame smoke test"
    (let [top (impl/make-top-level-env 'metaprob.infer)
          f (make-env top)]
      (env-bind! f "foo" 17)
      (is (= (env-lookup f "foo") 17))
      (is (= (env-lookup f "sub") builtin/sub)))))

(deftest frame-2
  (testing "match-bind smoke test"
    (let [top (impl/make-top-level-env 'metaprob.infer)
          f (make-env top)
          pat (from-clojure '[a b])]
      (match-bind pat (list 1 2) f)
      (is (= (env-lookup f "a") 1))
      (is (= (env-lookup f "b") 2)))))

(deftest tag-capture
  (testing "capture- and retrieve-tag-address smoke test"
    (let [root (trace/new-trace "root")
          q (capture-tag-address root root root)
          a (impl/metaprob-list "this" "that")
          r (resolve-tag-address (trace/pair q a))
          o2 (impl/metaprob-nth r 2)]
      (is (trace/trace? o2))
      (trace/trace-set o2 "value")
      (is (= (trace/trace-get o2) "value"))
      (is (= (trace/trace-get (trace/lookup root a)) "value"))
      (is (= (trace/trace-get root a) "value")))))




(defn mk_nil [] (trace/empty-trace))

(defn ez-call [prob-prog & inputs]
  (let [inputs (if (= inputs nil) '() inputs)
        [value score]
        (trace/metaprob-sequence-to-seq
         (infer prob-prog
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

(defn ez-eval [x]
  (let [[value score]
        (trace/metaprob-sequence-to-seq
         (infer-eval (from-clojure x)
                     (impl/make-top-level-env 'metaprob.infer)
                     (mk_nil)
                     (mk_nil)
                     (mk_nil)))]
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

;; Export a procedure i.e. use 'foreign' (clojure) version rather than
;; trying to compile the 'native' version (source code)

(deftest export-1
  (testing "export a procedure"
    (let [x 5
          m1 (gen [] x)
          m2 (impl/make-opaque m1)]
      (is (= (m2) (m1))))))

;; Lift a generate method up to a infer method

(deftest lift-1
  (testing "lift a generate method up to a infer method"
    (let [m (gen [argseq i t o]
                      (define [x y] argseq)
                      (tuple (+ x 1) 19))
          l (inf "testing" m)]
      (is (= (l 17 "z") 18)))))


;(deftest lift-and-call
;  (testing "can we lift a procedure and then call it"
;    (let [qq (impl/make-foreign-procedure "qq" (fn [argseq i t o]
;                                             [(+ (metaprob-nth argseq 0) (metaprob-nth argseq 1))
;                                              50]))
;          lifted (inf "lifted" qq)]
;      (let [[answer score] (infer lifted [7 8] nil nil nil)]
;        (is (= answer 15))
;        (is (= score 50))))))
