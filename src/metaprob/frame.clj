(ns metaprob.frame
  "Lexical environments, needed by gen macro."
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(define frame?
  (gen [obj]
    (if (trace? obj)
      (trace-has-subtrace? obj "*parent*")
      false)))

(define environment?
  (gen [obj]
    (or (frame? obj)
        (top-level-environment? obj))))

(define frame-parent
  (gen [frame]
    (trace-get frame "*parent*")))

(define make-env
  (gen [parent]
    (assert (environment? parent) parent)
    (mutable-trace "*parent*" parent)))

(define env-lookup
  (gen [env name]
    (if (frame? env)
      (if (trace-has? env name)
        (trace-get env name)
        (env-lookup (frame-parent env) name))
      ;; Top level environment
      (top-level-lookup env name))))

(define env-bind!
  (gen [env name val]
    (if (frame? env)
      (trace-set! env name val)
      (assert false "bad env-bind!"))))

;; match-bind! -
(defgen match-bind!
  "Overrides original prelude. Liberal in what it accepts: the input
  can be either a list or a tuple, at any level.
  `pattern` is a parse-tree trace (variable or tuple expression) - not a tuple.
  `input` is anything."

  [pattern input env]
    (case (trace-get pattern)
      "variable"
      (env-bind! env (trace-get pattern "name") input)
      "tuple"
      (block (define count (trace-count pattern))

             (define loup
               (gen [i cursor]
                 (cond (eq i count)
                       ;; We've reached the end of the patterns
                       (assert (empty-trace? cursor)
                               ["too many inputs"
                                (length input)
                                count
                                (clojure.core/map to-immutable
                                                  (to-immutable input))
                                pattern
                                env])

                       ;; The pattern [& x] matches anything
                       (and (eq i (sub count 2))
                            (eq (trace-get pattern i) "&"))
                       (match-bind! (trace-subtrace pattern (+ i 1))
                                    cursor
                                    env)

                       ;; Ensure that an input remains to match remaining pattern
                       (empty-trace? cursor)
                       (assert false
                               ["too few inputs"
                                (length input)
                                count
                                (clojure.core/map to-immutable
                                                  (to-immutable input))
                                pattern
                                env])

                       ;; Bind pattern to input, and continue
                       true
                       (block (match-bind! (trace-subtrace pattern i) (first cursor) env)
                              (loup (+ i 1) (rest cursor))))))
             (loup 0 (to-list input)))
      (do (pprint pattern)
          (assert false ["bad pattern" pattern input])))
    "return value of match-bind!")
