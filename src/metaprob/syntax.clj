(ns metaprob.syntax
  "This module is intended for import by metaprob code, particularly in
  builtins.clj. It defines the syntactic constructs to be used in
  metaprob programs."
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [metaprob.state :as state]
            [metaprob.trace :refer :all]
            [metaprob.sequence :refer :all]
            [metaprob.builtin-impl :as impl]))

(def set-value state/set-value)

(defmacro define
  "Like def, but allows patterns."
  ([pattern docstring rhs]
   `(define ~pattern ~rhs))

  ([pattern rhs]
   (letfn [(var-for-pattern [pat]
             (if (symbol? pat)
               pat
               (symbol (string/join "|"
                                    (map var-for-pattern pat)))))

           ;; Insert name into function expression, if any
           (form-def [name rhs]
             (let [rhs (if (and (seq? rhs)
                                (= (first rhs) 'gen))
                         `(~'named-generator ~name ~@(rest rhs))
                         rhs)]
               `(def ~name ~rhs)))

           ;; Returns a list [[var val] ...]
           ;; to be turned into, say, (block (define var val) ...)
           ;; or into (let [var val ...] ...)
           (explode-pattern [pattern rhs]
             (if (symbol? pattern)
               (list (form-def pattern rhs))
               (let [var (var-for-pattern pattern)]
                 (cons (form-def var rhs)
                       (mapcat (fn [subpattern i]
                                 (if (and (symbol? subpattern)
                                          (= (name subpattern) "_"))
                                   (list)
                                   (explode-pattern subpattern `(metaprob-nth ~var ~i))))
                               pattern
                               (range (count pattern)))))))]

     `(do ~@(explode-pattern pattern rhs)))))

(declare from-clojure from-clojure-pattern)

;; Run time

(defn free-vars-approximately
  "Recursively descends through nested vectors, sequences, and maps and
  returns a set of all the symbols found therein."
  [form]
  ;; This is incredibly crude, but it doesn't much matter if there are a
  ;; few junk names in the result, since they'll get filtered out
  (into #{}
        (filter symbol?)
        (tree-seq (some-fn vector? seq? map?)
                  #(if (map? %) (sequence cat %) (seq %))
                  form)))

;; This can fail with forward references to recursive functions

(defn make-generative [fun name exp-trace top-env names values]
  (let [proc-name (impl/trace-name exp-trace name)]               ;original metaprob
    (assert (= (trace-get exp-trace) "gen"))
    (trace-as-procedure (trace "name" proc-name
                               "generative-source" (** exp-trace)
                               "environment"
                               (if (empty? names)
                                 top-env
                                 (into {"*parent*" {:value top-env}}
                                       (map (fn [name value] [name {:value value}])
                                            names
                                            values)))
                               :value "prob prog")
                        ;; When called from clojure:
                        fun)))

;; Compile time

(defmacro named-generator [name params & body]
  {:style/indent 2}
  (let [fn-body (if (some #(= '& %) params)
                  (let [screwy (last params)]
                    `(let [~screwy (if (= ~screwy nil)
                                     '()
                                     ~screwy)]
                       (block ~@body)))
                  `(block ~@body))     ; cope with defines if any
        fn-exp `(fn ~@(if name `(~name) `())
                  ~params
                  ~fn-body)
        exp-trace (from-clojure `(~'gen ~params ~@body))
        names (vec (set/intersection (free-vars-approximately fn-exp)
                                     (set (keys &env))))]
    ;;(if (not (empty? names))
    ;;  (impl/metaprob-print ["the names are:" names]))
    `(make-generative ~fn-exp
                      '~name
                      '~exp-trace
                      (impl/make-top-level-env *ns*)
                      ;; Or: {name val name val ... "*parent*" top-env}  ?
                      '~(seq (map str names))
                      ~names)))

(defmacro gen
  "like fn, but for metaprob procedures"
  {:style/indent 1}
  [params & body]
  `(named-generator nil ~params ~@body))

;; Oddly, the source s-expressions don't seem to answer true to list?

(defmacro block
  "like do, but for metaprob - supports local definitions"
  [& forms]
  (letfn [(definition? [form]
            (and (seqable? form)
                 (symbol? (first form))
                 ;; Can't compare to 'define, wrong namespace
                 (= (name (first form)) "define")
                 (do (assert (= (count form) 3)) true)))
          (definition-pattern [form]
            (second form))
          (definition-rhs [form]
            (nth form 2))
          (generator-definition? [form]
            (and (definition? form)
                 (symbol? (definition-pattern form))
                 (let [rhs (definition-rhs form)]
                   (and (seqable? rhs)
                        (symbol? (first rhs))
                        (= (name (first rhs)) "gen")))))
          (qons [x y]
            (if (list? y)
              (conj y x)
              (conj (concat (list) y) x)))
          (process-definition [form]
            (assert generator-definition? form)
            (let [rhs (definition-rhs form)       ; a gen-expression
                  prog-pattern (definition-pattern rhs)
                  prog-body (rest (rest rhs))]
              ;; (name [args] body1 body2 ...) as in letfn
              (qons (definition-pattern form)
                    (list prog-pattern
                          (qons 'block prog-body)))))

          ;; Returns let-list [var exp var exp ...]
          (explode-pattern [pattern rhs]
            (if (symbol? pattern)
              (list pattern rhs)
              (let [var '?subject?]
                (cons var
                      (cons rhs
                            (mapcat (fn [subpattern i]
                                      (if (= subpattern '_)
                                        (list)
                                        (explode-pattern subpattern
                                                         `(metaprob-nth ~var ~i))))
                                    pattern
                                    (range (count pattern))))))))

          (block-to-body [forms]
            (if (empty? forms)
              '()
              (let [here (first forms)
                    more (block-to-body (rest forms))]    ; list of forms
                (if (definition? here)
                  (let [pattern (definition-pattern here)
                        rhs (definition-rhs here)]
                    ;; A definition must not be last expression in a block
                    (if (empty? (rest forms))
                      (print
                       (format
                        "** Warning: Definition of %s occurs at end of block\n"
                        pattern)))
                    (list
                     (if (generator-definition? here)
                       (let [spec (process-definition here)
                             next (first more)]
                         (if (and (list? next)
                                  (= (first next) `letfn))
                           (do (assert (empty? (rest more)))
                               ;; next = (letfn [...] ...)
                               ;;    (letfn [...] & body)
                               ;; => (letfn [(name pattern & prog-body) ...] & body)
                               (let [[_ specs & body] next]
                                 (qons `letfn
                                       (qons (vec (cons spec specs))
                                             body))))
                           ;; next = (first more)
                           (qons `letfn
                                 (qons [spec]
                                       more))))
                       ;; Definition, but not of a function
                       (qons `let
                             (qons (vec (explode-pattern pattern rhs))
                                   more)))))
                  ;; Not a definition
                  (qons here more)))))

          (formlist-to-form [forms]
            (assert (seqable? forms))
            (if (empty? forms)
              `nil
              (if (empty? (rest forms))
                (first forms)
                (if (list? forms)
                  (qons `do forms)
                  (qons `do (concat (list) forms))))))]
    (formlist-to-form (block-to-body forms))))

;(defmacro tuple [& members]
;  `(set-value ~(zipmap (range (count members))
;              (map (fn [x] `(new-trace ~x)) members))))

(defmacro &this []
  "(&this) makes no sense when clojure-compiled")

(defmacro mp-splice [x]
  (assert false ["splice used in evaluated position!?" x]))

;; unquote is a name conflict with clojure

(defmacro mp-unquote [x]
  (assert false ["unquote used in evaluated position!?" x]))

; -----------------------------------------------------------------------------
; Convert a clojure expression to a metaprob parse tree.

; Assumes that the input was generated by the to-clojure converter.
; The parse tree is always a hereditarily immutable trace, so that it
; can be hashed.

;; TBD: This file assumes it knows what the represenation of an
;; immutable trace is.  That's not very good data abstraction.  Fix.

; These could all be marked ^:private

(defn from-clojure-seq [sequ val]    ; seq of clojure expressions
  (set-value (zipmap (range (count sequ))
                     (map from-clojure sequ))
             val))

;; Trace       => clojure               => trace
;; (x)->{a;b;} => (gen [x] (block a b)) => (x)->{a;b;}
;; (x)->{a;}   => (gen [x] (block a))   => (x)->{a;}
;; (x)->a      => (gen [x] a)           => (x)->a
;;
;; safe abbreviation:
;; (x)->{a;b;} => (gen [x] a b) => (x)->{a;b;}

(defn from-clojure-gen [exp]
  (let [[_ pattern & body] exp]
    (let [body-exp (if (= (count body) 1)
                     (first body)
                     (cons 'block body))]
      (trace :value "gen"
             "pattern" (** (from-clojure-pattern pattern))
             "body" (** (from-clojure body-exp))))))

(defn from-clojure-pattern [pattern]
  (if (symbol? pattern)
    (if (= pattern '&)
      (trace :value "&")
      (trace :value "variable" "name" (str pattern)))
    (do (assert (vector? pattern) pattern)
        (to-immutable
         (trace-from-subtrace-seq (map from-clojure-pattern pattern) "tuple")))))

(defn from-clojure-if [exp]
  (let [[_ pred thn els] exp]
    (trace :value "if"
           "predicate" (** (from-clojure pred))
           "then" (** (from-clojure thn))
           "else" (** (from-clojure els)))))

(defn from-clojure-block [exp]
  (assert (not (empty? exp))
          ["empty block" exp])
  (from-clojure-seq (rest exp) "block"))

;; Cf. name-for-definiens in infer.clj.

(defn from-clojure-definition [exp]
  (let [[_ pattern rhs] exp
        key (if (and (symbol? pattern)
                     (not (= pattern '_))
                     (not (= pattern 'pattern)))
              (str pattern)
              "definiens")]
    (trace :value "definition"
           "pattern" (** (from-clojure-pattern pattern))
           key (** (from-clojure rhs)))))

(defn from-clojure-application [exp]
  (assert (not (empty? exp))
          ["empty application" exp])
  (from-clojure-seq exp "application"))

(defn from-clojure-tuple [exp]
  (from-clojure-application (cons 'tuple exp)))

(defn from-clojure-literal [value]
  (trace :value "literal"
         "value" value))

(defn from-clojure-and [exp]
  (letfn [(expand [forms]
            (cond (empty? forms)
                    'true
                  (empty? (rest forms))
                    (first forms)
                  true
                    (list 'if
                        (first forms)
                        (expand (rest forms))
                        'false)))]
    (from-clojure (expand (rest exp)))))

(defn from-clojure-or [exp]
  (letfn [(expand [forms]
            (cond (empty? forms)
                    'false
                  (empty? (rest forms))
                    (first forms)
                  true
                    (let [g '_or_temp_]
                      (list 'block
                          (list 'define g (first forms))
                          (list 'if
                                g
                                g
                                (expand (rest forms)))))))]
    (from-clojure (expand (rest exp)))))

(defn from-clojure-case [exp]
  (let [[_ subj & clauses] exp]
    (let [g '_case_temp_]
      (letfn [(expand [forms]
                (cond (empty? forms)
                      'nil
                      (empty? (rest forms))
                      (first forms)
                      true
                      (list 'if
                            (list 'eq g (first forms))
                            (first (rest forms))
                            (expand (rest (rest forms))))))]
        (from-clojure
         (list 'block
               (list 'define g subj)
               (expand clauses)))))))

(defn from-clojure-cond [exp]
  (let [[_ & forms] exp]
    (letfn [(expand [forms]
              (if (empty? forms)
                'nil
                (list 'if
                      (first forms)
                      (first (rest forms))
                      (expand (rest (rest forms))))))]
      (from-clojure (expand forms)))))

;; N.b. strings are seqable

(defn literal-exp? [exp]
  (or (number? exp)
      (string? exp)
      (boolean? exp)
      (keyword? exp)
      (= exp nil)))

;; Don't create variables with these names...
;;   (tbd: look for :meta on a Var in this namespace ??)
(def prohibited-names #{"block" "gen" "define" "if"})

(defn from-clojure-1 [exp]
  (cond (vector? exp) (from-clojure-tuple exp)    ;; Pattern - shouldn't happen

        (literal-exp? exp)    ;; including string
          (from-clojure-literal exp)

        (symbol? exp)
        (let [s (str exp)]
          (assert (not (contains? prohibited-names s)) exp)
          (trace :value "variable"
                 "name" s))

        ;; I don't know why this is sometimes a non-list seq.
        ;; TBD: check that (first exp) is a non-namespaced symbol.
        (seq? exp) (case (first exp)
                     gen (from-clojure-gen exp)
                     probprog  (from-clojure-gen exp)    ;DEPRECATED
                     program  (from-clojure-gen exp)     ;DEPRECATED
                     if (from-clojure-if exp)
                     block (from-clojure-block exp)
                     mp-splice (trace :value "splice"
                                      "expression" (** (from-clojure exp)))
                     mp-unquote (trace :value "unquote"
                                       "expression" (** (from-clojure exp)))
                     define (from-clojure-definition exp)
                     ;; Syntactic sugar
                     and (from-clojure-and exp)
                     or (from-clojure-or exp)
                     case (from-clojure-case exp)
                     cond (from-clojure-cond exp)
                     ;; else
                     (from-clojure-application exp))
        ;; Literal
        true (assert false ["bogus expression" exp])))

(defn from-clojure [exp]
  (let [answer (from-clojure-1 exp)]
    (assert (trace? answer) ["bad answer" answer])
    (assert (trace-has? answer) ["no value" answer])
    answer))
