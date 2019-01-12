;; A meta-circular Metaprob interpreter.

(ns metaprob.compositional
  (:refer-clojure :only [declare ns name])
  (:require [metaprob.syntax :refer :all]
            [clojure.pprint :as pp]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.prelude :refer :all]
            [metaprob.frame :refer :all]))

;; ----------------------------------------------------------------------------

(declare infer-apply-native infer-apply-foreign
         infer-apply infer-eval infer-eval-sequence)




(define compile-mp-proc
  (clojure.core/fn [proc]
    (clojure.core/with-meta
      (clojure.core/fn [& args] (nth (infer-apply proc (clojure.core/vec args) {} {} false) 0))
      (unbox proc))))


;; Output-is-not-an-input version
(define third  (gen [l] (first (rest (rest l)))))
(define fourth (gen [l] (first (rest (rest (rest l))))))

;; Main entry point: an `apply` that respects interventions
;; and constraints, records choices made, and computes scores.

;; Output-in-not-an-input version

(define infer-apply
  (gen [proc inputs intervene target output?]
    (assert (trace? intervene) ["bad intervene" intervene])
    (assert (trace? target) ["target" target])
    (assert (boolean? output?) output?)

    (if (and (compound? proc) (contains? proc :implementation))
      ;; Proc is a special inference procedure returned by `inf`.
      ;; Return the value+output+score that the implementation computes.
      (block
       (define imp (get proc :implementation))
       (imp inputs intervene target output?))

      (cond
        ;; Bypass interpreter when there is no need to use it.  Was once
        ;; necessary in order for map and apply to work properly; it might
        ;; be possible to remove this check now that we have
        ;; *ambient-interpreter*.
        (and (fn? proc)
             (empty? intervene)
             (empty? target)
             (not output?))
        (infer-apply-foreign proc inputs intervene target output?)

        ;; Native (interpreted) generative procedure.
        (native-procedure? proc)
        (infer-apply-native proc inputs intervene target output?)

        ;; Foreign (opaque, compiled) procedure.
        (fn? proc)
        (infer-apply-foreign proc inputs intervene target output?)

        ;; Otherwise, this is not a procedure we can interpret.
        true
        (block (pprint proc)
               (error "infer-apply: not a procedure" proc))))))

;; Invoke a 'foreign' generative procedure, i.e. one written in
;; clojure (or Java)

(define infer-apply-foreign
  (gen [proc inputs intervene target output?]
    ;; 'Foreign' generative procedure
    (define value (generate-foreign proc inputs))
    (define ivalue (if (trace-has-value? intervene)
                     (trace-value intervene) value))
    (if (and (trace-has-value? target)
             (not (= (trace-value target) ivalue)))
      [(trace-value target) {:value (trace-value target)} negative-infinity]
      [ivalue {} 0])))

;; Invoke a 'native' generative procedure, i.e. one written in
;; metaprob, with inference mechanics (traces and scores).
(define infer-apply-native
  (gen [proc inputs intervene target output?]
    (define source (get proc :generative-source))
    (define environment (get proc :environment))
    (define new-env (make-env environment))
    ;; Extend the enclosing environment by binding formals to actuals
    (match-bind! (second source) ; pattern. (source is of form '(gen [...] ...)
                 inputs
                 new-env)
    (infer-eval (cons 'block (rest (rest source))) ; body with implicit `block`
                new-env
                intervene
                target
                output?)))

(define infer-subeval
  (gen [sub-exp adr env intervene target output?]
    (define [v o s]
      (infer-eval sub-exp
                  env
                  (maybe-subtrace intervene adr)
                  (maybe-subtrace target adr)
                  output?))
    [v (maybe-set-subtrace {} adr o) s]))

(define infer-eval-expressions
  (gen [exp env intervene target output?]
    (assert (> (count exp) 0) exp)
    (define [_ answer]
      (clojure.core/reduce
        (gen [[i [v o s]] next]
          (define [sub-v sub-o sub-s]
            (infer-subeval next i env intervene target output?))
          [(+ i 1) [(clojure.core/conj v sub-v)
                    (trace-merge o sub-o)
                    (+ s sub-s)]])
        [0 [[] {} 0]]
        exp))
    answer))

; Question:
; In the code {(uniform-sample ["key1" "key2" "key3"]) (flip 0.5)}, does resampling the key change the "control flow"?
; Or can we reuse the flip even after the key changes?

(define infer-eval-map
  (gen [exp env intervene target output?]
    (assert (map? exp) exp)
    (assert (not (empty? exp)) exp)
    (define [_ answer]
      (clojure.core/reduce-kv
        (gen [[i [v o s]] next-key next-val]
          (define [key-v key-o key-s]
            (infer-subeval next-key (str "key" i) env intervene target output?))
          (define [val-v val-o val-s]
            (infer-subeval next-val (str key-v) env intervene target output?))
          [(+ i 1)
           [(clojure.core/conj v [key-v val-v])
            (trace-merge (trace-merge o key-o) val-o)
            (+ s key-s val-s)]])
        [0 [{} {} 0]]
        exp))
    answer))


;; Evaluate a subexpression (by reduction)
(define infer-eval
  (gen [exp env intervene target output?]
    ;; (assert (trace? exp) ["bad expression - eval" exp])
    (assert (environment? env) ["bad env - eval" env])
    (assert (trace? intervene) ["bad intervene" intervene])
    (assert (trace? target) ["bad target" target])
    (assert (boolean? output?) output?)

    (define [v output score]
      ;; Dispatch on type of expression
      (cond
        ; is it a variable?
        (symbol? exp)
        [(env-lookup env exp) {} 0] ; TODO: strip namespace?

        ; is it a literal?
        (or (not (compound? exp)) (empty? exp))
        [exp {} 0]

        ; is it a vector?
        (vector? exp)
        (infer-eval-expressions exp env intervene target output?)

        ; is it a map?
        (map? exp)
        (infer-eval-map exp env intervene target output?)

        ; is it a quote?
        (= (first exp) 'quote)
        [(second exp) {} 0]

        ; is it an if?
        (= (first exp) 'if)
        (block
          (define [pred-val pred-output pred-score]
            (infer-subeval
              (second exp)
              "predicate" env intervene target output?))
          (define [key subexp] (if pred-val ["then" (third exp)] ["else" (fourth exp)]))
          (define [val output score]
            (infer-subeval
              subexp key
              env intervene target output?))

          [val
           (trace-merge pred-output output)
           (+ pred-score score)])

        ; is it a definition?
        (= (name (first exp)) "define")
        ; '(define pattern exp)
        (block
          (define key (name-for-definiens (second exp)))
            (define [val output score]
              (infer-subeval (third exp) key env intervene target output?))
            [(match-bind! (second exp) val env)
                output
                score])

        ; is it a block?
        (= (name (first exp)) "block")
        (block
          (define [values output score]
            (infer-eval-expressions (rest exp) (make-env env) intervene target output?))
          [(clojure.core/last values) output score])

        ; is it a gen?
        (= (name (first exp)) "gen")
        [(compile-mp-proc
           {:name (trace-name exp)
            :generative-source exp, ; (cons 'gen (cons (second exp) (map mp-expand (rest (rest exp)))))
            :environment env})

           ;(clojure.core/with-meta
           ;(clojure.core/fn [& args]
           ;  (nth (infer-apply {:name (trace-name exp),
           ;                :generative-source (mp-expand exp),
           ;                :environment env} (clojure.core/vec args)
           ;               {} {} false) 0))
           ;{:name (trace-name exp)
           ;:generative-source (mp-expand exp)
           ;:environment env})
         {}
         0]


        ; otherwise, it must be an application
        true
        (block
         (define [values output score]
                 (infer-eval-expressions exp env intervene target output?))

               (define result-key
                 (application-result-key (first exp)))

               (define [result suboutput subscore]
                 (infer-apply (first values)
                              (rest values)
                              (maybe-subtrace intervene result-key)
                              (maybe-subtrace target result-key)
                              output?))
               [result
                (if output?
                  (maybe-set-subtrace output result-key suboutput)
                  output)
                (+ subscore score)])))

    (assert (trace? output) output)

    (define ivalue (if (trace-has-value? intervene)
                     (trace-value intervene) v))
    (define tvalue (if (trace-has-value? target)
                     (trace-value target) v))

    (cond
      ; intervention with no disagreeing target
      (and (trace-has-value? intervene)
           (or (not (trace-has-value? target))
               (= ivalue tvalue)))
      [ivalue
       (if (empty? output) {} {:value ivalue})
       0]

      ; target and value (from intervention or execution) disagree
      (and (trace-has-value? target)
           (not (= ivalue tvalue)))
      [tvalue
       (trace-set-value output tvalue)
       negative-infinity]

      ; in all other cases, the existing values work fine:
      true
      [v output score])))
