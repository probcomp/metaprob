;; See doc/about-the-prelude.md

(ns metaprob.prelude
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all]))

;; -----------------------------------------------------------------------------
;; Interpreter utilities

(define maybe-subtrace
  (gen [tr adr]
    (if (trace-has-subtrace? tr adr)
      (trace-subtrace tr adr)
      {})))

(define maybe-set-subtrace
  (gen [output key suboutput]
    (assert (trace? output) "bad output")
    (if suboutput
      (block (assert (trace? suboutput) ["bad suboutput" key suboutput])
             (if (empty? suboutput)
               output
               (trace-set-subtrace output key suboutput)))
      output)))

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
    (and (compound? thing)
         (contains? thing :generative-source)
         (contains? thing :environment))))

;; -----------------------------------------------------------------------------
;; Inf-based utilities.

(define opaque
  (gen [name proc]
    (inf name
         proc   ;model (generative procedure)
         (gen [inputs intervene target output?]
           ;; Ignore the traces.
           (infer-apply proc inputs {} {} false)))))

(define apply
  (clojure.core/with-meta
    ;; Kludge
    (gen [proc inputs]
      (clojure.core/apply proc inputs))
    (unbox
      (inf "apply"
          clojure.core/apply   ;model (generative procedure)
          (gen [inputs intervene target output?]
            ; TODO: allow apply to be n-ary, with last argument a collection
            (infer-apply (first inputs) (second inputs)) intervene target output?)))))


;; map defined using inf (instead of with-address)

(define map-issue-20
  (inf "map"
       nil                              ;no model
       (gen [[fun sequ] intervene target output?]
         (define re
                  (gen [i l]
                    (if (empty? l)
                      [(if (vector? sequ) [] l) {} 0]
                      (block (define [value suboutput subscore]
                               (infer-apply fun
                                            [(first l)]
                                            ;; advance traces by address i
                                            (maybe-subtrace intervene i)
                                            (maybe-subtrace target i)
                                            output?))

                             ;; Recur over rest of list
                             (define [values output score]
                               (re (+ i 1)
                                   (rest l)))
                             [(cons value values)
                              (maybe-set-subtrace output i suboutput)
                              (+ subscore score)]))))

                ;; Fire it up
                (re 0 (to-list sequ)))))

(define map map-issue-20)

(define replicate
  (gen [n f]
    (map (gen [i] (f))
         (range n))))
