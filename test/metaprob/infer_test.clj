(ns metaprob.infer-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace]
            [metaprob.syntax :as syntax :refer :all]
            [metaprob.builtin-impl :as impl]
            [metaprob.builtin :as builtin]
            [metaprob.infer :refer :all :exclude [apply]]))

(def top (impl/make-top-level-env 'metaprob.infer))

(deftest frame-1
  (testing "frame smoke test"
    (let [f (make-env top)]
      (env-bind! f "foo" 17)
      (is (= (env-lookup f "foo") 17))
      (is (= (env-lookup f "sub") builtin/sub)))))

(deftest frame-2
  (testing "match-bind smoke test"
    (let [f (make-env top)
          pat (from-clojure-pattern '[a b])]
      (match-bind pat (list 1 2) f)
      (is (= (env-lookup f "a") 1))
      (is (= (env-lookup f "b") 2)))))

(deftest tag-capture
  (testing "capture- and retrieve-tag-address smoke test"
    (let [root (trace/new-trace "root")
          cap (capture-tag-address root root root)]
      (is (= (trace/trace-count cap) 3))
      (let [a (trace/addr "that" "those")
            quasi (trace/pair cap a)      ; /*this/that/those/ ?
            [i t o] (resolve-tag-address quasi)]
        (is (trace/trace? i))
        (trace/trace-set i "value")
        (is (= (trace/trace-get i) "value"))

        (let [again (trace/trace-subtrace root a)]
          (is (= (trace/trace-get again) "value"))
          (is (= (trace/trace-get root a) "value")))))))




(defn mk_nil [] (trace/empty-trace))

(defn ez-call [prob-prog & inputs]
  (let [inputs (if (= inputs nil) '() inputs)
        [value score]
        (trace/metaprob-sequence-to-seq
         (infer-apply prob-prog
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
                     top
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
    (let [m (gen [inputs i t o]
                      (define [x y] inputs)
                      (trace/tuple (+ x 1) 19))
          l (inf "testing" m)]
      (is (= (l 17 "z") 18)))))


;(deftest lift-and-call
;  (testing "can we lift a procedure and then call it"
;    (let [qq (impl/make-foreign-procedure "qq" (fn [inputs i t o]
;                                             [(+ (metaprob-nth inputs 0) (metaprob-nth inputs 1))
;                                              50]))
;          lifted (inf "lifted" qq)]
;      (let [[answer score] (infer-apply lifted [7 8] nil nil nil)]
;        (is (= answer 15))
;        (is (= score 50))))))

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
    (is (= (ez-eval '(case 1 2 3 1 4)) 4))))


(deftest intervene-1
  (testing "simple intervention"
    (let [form (from-clojure '(block 17 19))
          [value1 _] (infer-eval form top nil nil nil)]
      (is (= value1 19))
      (let [intervene (builtin/empty-trace)]
        (trace/trace-set intervene 1 23)
        (let [[value2 _] (infer-eval form top intervene nil nil)]
          (is (= value2 23)))))))


(deftest intervene-2
  (testing "output capture, then intervention"
    (let [form (from-clojure '(block (add 15 2) (sub 21 2)))
          output (builtin/empty-trace)
          [value1 _] (infer-eval form top nil nil output)]
      (is (= value1 19))
      (let [intervene (builtin/empty-trace)
            addresses (builtin/addresses-of output)]
        ;; (builtin/pprint output)
        (doseq [a addresses]
          (trace/trace-set intervene a 23))
        (let [[value2 _] (infer-eval form top intervene nil nil)]
          (is (= value2 23)))))))
