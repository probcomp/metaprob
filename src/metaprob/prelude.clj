;; See doc/about-the-prelude.md

(ns metaprob.prelude
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all]))

;; -----------------------------------------------------------------------------
;; Interpreter utilities

(define maybe-subtrace-1
  (gen [x] x))

(define maybe-subtrace
  (gen [tr adr]
    (or (trace-subtrace tr adr) {})))

(define maybe-set-subtrace
  (gen [output adr suboutput]
    (if (empty? suboutput)
      (trace-clear-subtrace output adr)
      (trace-set-subtrace output adr suboutput))))

;; Utilities for interpreter

;; This is used to compute the key under which to store the value that
;; is the binding of a definition.

(define name-for-definiens
  (gen [pattern]
    (if (or (vector? pattern) (= pattern '_))
      "definiens"
      (str pattern))))

;; Get the key to use for storing the result of a procedure call.
(define application-result-key
  (gen [exp]
    (if (symbol? exp) (str exp) "call")))

(define native-procedure?
  (gen [thing]
    (and (contains? thing :generative-source)
         (contains? thing :environment))))

;; -----------------------------------------------------------------------------
;; Inf-based utilities.

(define opaque
  (gen [name proc]
    (inf name
         proc   ;model (generative procedure)
         (gen [inputs ctx]
           ;; Ignore the traces.
           (infer-apply proc inputs {} {} false)))))


(define apply
  (clojure.core/with-meta
    ;; Kludge
    clojure.core/apply
    {:apply? true}))


    ;(unbox
    ;  (inf "apply"
    ;      clojure.core/apply   ;model (generative procedure)
    ;      (gen [inputs ctx]
    ;        ; TODO: allow apply to be n-ary, with last argument a collection
    ;        ; TODO: should use the currently active interpreter; whichever interpreter is evaluating the call to apply.
    ;        (pprint ["Calling infer-apply with the context I was passed:" ctx])
    ;        ; Note: this returns a trace, rather than a context!
    ;        ; We need two versions of infer-apply: a context version and a trace version.
    ;        ; I should figure out how *ambient-interpreter* and the tower of interpreters
    ;        ; is supposed to work...
    ;        (infer-apply (first inputs) (second inputs) {} {} true ctx))))))
    ;        ; (assert false "Tracing `apply` is currently unimplemented."))))))
    ;        ;(infer-apply (first inputs) (second inputs) ctx))))))

;
;;; map defined using inf (instead of with-address)
;
;; TODO: This also depends on the ability to call infer-apply
;(define map-issue-20
;  (inf "map"
;       nil                              ;no model
;       (gen [[fun sequ] ctx]
;         (define re
;           (gen [i l]
;             (if (empty? l)
;               [(if (vector? sequ) [] l) ctx 0]
;               (block (define [value suboutput subscore]
;                        (infer-apply fun
;                                     [(first l)]
;                                     ;; advance traces by address i
;                                     (maybe-subtrace intervene i)
;                                     (maybe-subtrace target i)
;                                     output?))
;                      ;; Recur over rest of list
;                      (define [values output score]
;                        (re (+ i 1)
;                            (rest l)))
;                      [(cons value values)
;                       (maybe-set-subtrace output i suboutput)
;                       (+ subscore score)]))))
;
;                ;; Fire it up
;                (re 0 (to-list sequ)))))
;
;(define map
;  (gen [f l]
;    (with-explicit-tracer t
;      (define helper
;        (gen [l i]
;          (if (empty? l)
;            '()
;            (cons (t i f (first l))
;                  (helper (rest l) (+ i 1))))))
;      (helper l 0))))
;
;; (define map map-issue-20)
;
;(define replicate
;  (gen [n f]
;    (with-explicit-tracer t
;      (define helper
;        (gen [i acc]
;          (if (= i n) acc (helper (+ i 1) (cons (t i f) acc)))))
;      (helper 0 '()))))

(define map
  (gen [f l]
    (with-explicit-tracer t
      (clojure.core/doall
        (clojure.core/map-indexed
          (gen [i x] (t i f x)) l)))))

(define replicate
  (gen [n f]
    (map (gen [_] (f)) (range n))))