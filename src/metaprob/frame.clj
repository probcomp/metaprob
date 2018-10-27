(ns metaprob.frame
  (:refer-clojure :only [declare ns atom swap!])
  (:require
    [metaprob.syntax :refer :all]
    [metaprob.builtin :refer :all]
    [metaprob.prelude :refer :all]))

;; ----------------------------------------------------------------------------
;; Lexical environments, needed by gen macro.
;; TBD: Move to prelude?

(define frame?
  (gen [obj]
    (and (compound? obj) (contains? obj :parent))))

(define environment?
  (gen [obj]
    (or (frame? obj)
        (top-level-environment? obj))))

(define frame-parent
  (gen [frame]
    (get frame :parent)))

(define make-env
  (gen [parent]
    (assert (environment? parent) parent)
    (atom {:parent parent})))

(define env-lookup
  (gen [env name]
    (if (frame? env)
      (if (contains? env name)
        (get env name)
        (env-lookup (frame-parent env) name))
      ;; Top level environment
      (top-level-lookup env name))))

(define env-bind!
  (gen [env name val]
    (if (frame? env)
      (swap! env assoc name val)
      (assert false "bad env-bind!"))))

;; match-bind! - overrides original prelude.
;; Liberal in what it accepts: the input can be either a list or a
;; tuple, at any level.

(define match-bind!
  ;; pattern is a symbol or vector
  ;; input is anything.
  (gen [pattern input env]
    (cond
      (symbol? pattern)
      (env-bind! env pattern input)

      (= (representation pattern) :vector)
      (block
        (define n (count pattern))
        (define loup
          (gen [i cursor]
            (cond (= i n)
                  ;; We've reached the end of the patterns
                  (assert (empty? cursor)
                          ["too many inputs"
                                (count input)
                                n
                                input
                                pattern
                                env])

                       ;; The pattern [& x] matches anything
                       (and (= i (- n 2))
                            (= (pattern i) '&))
                       (match-bind! (pattern (+ i 1))
                                    cursor
                                    env)

                       ;; Ensure that an input remains to match remaining pattern
                       (empty? cursor)
                       (assert false
                               ["too few inputs"
                                (count input)
                                n
                                input
                                pattern
                                env])

                       ;; Bind pattern to input, and continue
                       true
                       (block (match-bind! (pattern i) (first cursor) env)
                              (loup (+ i 1) (rest cursor))))))
             (loup 0 (to-list input)))

      true
      (do (pprint pattern)
          (assert false ["bad pattern" pattern input])))
    "return value of match-bind!"))

