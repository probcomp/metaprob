(ns dontknow.builtin
  (:require [dontknow.trie :refer :all]))

; Builtins used in trace-choices.vnts and other extant metaprob sources:
;   trace_has_key
;   trace_get
;   trace_has
;   trace_set
;   trace_set_subtrace_at
;   lookup
;   mk_nil
;
;   pprint  [clojure conflict - takes a trace]
;   error
;   first rest   [clojure conflict]
;   last    [clojure conflict]
;   range   [clojure conflict]
;   length
;   map   [clojure conflict]
;   list_to_array
;   add (from metaprob '+')
;   add (from =)
;   eq
;   neq

;   name_for_definiens
;   make_env
;   match_bind      - extends an environment (implemented as trace).
;   env_lookup
;   capture_tag_address   - for 'this'
;   resolve_tag_address   - for with_address

;   interpret    -- what is this?
;   interpret_prim

(defn make-program [fun params body]
  (with-meta fun {:program true :params params :body body}))

(defmacro program
  "like fn, but for metaprob programs"
  [params & body]
  `(make-program (fn ~params (block ~@body))
                 '~params
                 '~body))

(defmacro block
  "like do, but for metaprob - supports local definitions"
  [& forms]
  (letfn [(definition? [form]
            (and (list? form)
                 (= (first form) 'def)))
          (definition-pattern [form]
            (second form))
          (definition-rhs [form]
            (nth form 2))
          (program-definition? [form]
            (and (definition? form)
                 (symbol? (definition-pattern form))
                 (let [rhs (definition-rhs form)]
                   (and (list? rhs)
                        (= (first rhs) 'program)))))
          (qons [x y]
            (if (list? y)
              (conj y x)
              (conj (concat (list) y) x)))
          (process-definition [form]
            (assert program-definition? form)
            (let [rhs (definition-rhs form)       ;a program-expression
                  prog-pattern (definition-pattern rhs)
                  prog-body (rest (rest rhs))]
              (qons (definition-pattern form)
                    (qons prog-pattern
                          prog-body))))

          (block-to-body [forms]
            (if (empty? forms)
              '()
              (let [more (block-to-body (rest forms))]    ; list of forms
                (if (definition? (first forms))
                  (let [pattern (definition-pattern (first forms))
                        rhs (definition-rhs (first forms))]
                    ;; A definition must not be last expression in a block
                    (if (empty? (rest forms))
                      (print (format "** Warning: Definition of ~s occurs at end of block\n"
                                     pattern)))
                    (if (program-definition? (first forms))
                      (let [spec (process-definition (first forms))
                            more1 (first more)]
                        (if (and (list? more1)
                                 (= (first more1) 'letfn))
                          (do (assert (empty? (rest more)))
                              ;; more1 = (letfn [...] ...)
                              ;;    (letfn [...] & body)
                              ;; => (letfn [(name pattern & prog-body) ...] & body)
                              (let [[_ specs & body] more1]
                                (list             ;Single form
                                 (qons 'letfn
                                       (qons (vec (cons spec specs))
                                             body)))))
                          ;; more1 = something else
                          (list                   ;Single form
                           (qons 'letfn
                                 (qons [spec]
                                       more)))))
                      ;; Definition, but not of a function
                      ;; (first forms) has the form (def pattern rhs)
                      (let [more1 (first more)]
                        (if (and (list? more1)
                                 (= (first more1) 'let))
                          ;; Combine two lets into one
                          (do (assert (empty? (rest more)))
                              (let [[_ specs & body] more1]
                                (list             ;Single form
                                 (qons 'let
                                       (qons (vec (cons pattern (cons rhs specs)))
                                             body)))))
                          (list                   ;Single form
                           (qons 'let
                                 (qons [pattern rhs]
                                       more)))))))
                  ;; Not a definition
                  (qons (first forms) more)))))

          (formlist-to-form [forms]
            (assert (seq? forms))
            (if (empty? forms)
              'nil
              (if (empty? (rest forms))
                (first forms)
                (if (list? forms)
                  (qons 'do forms)
                  (qons 'do (concat (list) forms))))))]
    (formlist-to-form (block-to-body forms))))


(defn tracify [x]
  (if (trie? x)
    x
    (let [m (meta x)]
      (if (map? m)
        (get m :trace)
        nil))))

(defn eq [x y] (= x y))
(defn neq [x y] (not (= x y)))
(defn add [x y]
  (if (and (number? x) (number? y))
    (+ x y)
    (if (and (seq? x) (seq? y))
      (concat x y)
      (assert false "bad place to be"))))

(defn gt [x y] (> x y))
(defn gte [x y] (>= x y))
(defn lt [x y] (< x y))
(defn lte [x y] (<= x y))

; Used in prelude.vnts:
;   is_metaprob_array
;     The null array has no value and no subtraces; same as metaprob nil.
;     Hmm.

(defn length [x]
  ;; if x is a trie do something special?
  (count x))

(defn first-noncolliding [mp-list]
  (if (trace? mp-list)
    (value mp-list)
    (first mp-list)))

(defn rest-noncolliding [mp-list]
  (if (trace? mp-list)
    (subtrie mp-list "rest")
    (rest mp-list)))

(defn last-noncolliding [mp-list]
  (if (trace? mp-list)
    (if (has-subtrie? mp-list "rest")
      (if (not (has-subtrie? (subtrie mp-list "rest") "rest"))
         mp-list
         (last-noncolliding (subtrie mp-list "rest")))
      mp-list)
    (last mp-list)))

(defn empty?-noncolliding [mp-list]
  (not (has-subtrie? mp-list "rest")))

(defn mk_nil [] (new-trie))                 ; {{ }}

(defn list_to_array [mp-list]
  (let [arr (mk_nil)]
    (letfn [(r [mp-list n]
              (if (empty?-noncolliding mp-list)
                arr
                (do (set-value-at! arr n (first-noncolliding mp-list))
                    (r (rest-noncolliding mp-list)
                       (+ n 1)))))]
      (r mp-list 0))))

(defn map-noncolliding [mp-fn mp-seq]
  ;; Do something - need to thread the trace through
  0)

(defn pair [thing mp*list]
  (trie-from-map {:rest mp*list} thing))

; Copied from prelude.clj
(defn _range [n k]
  (if (gte k n) (mk_nil) (pair k (_range n (add k 1)))))

(defn range-noncolliding [n]
  (_range n 0))

(defn trace_get [tr] (value tr))        ; *e
(defn trace_has [tr] (has-value? tr))
(defn trace_set [tr val]            ; e[e] := e
  (set-value! tr val))
(defn trace_set_at [tr addr val] (set-value-at! tr addr val))
(defn trace_set_subtrace_at [tr addr sub] (set-subtrie-at! tr addr sub))
(defn trace_has_key [tr key] (has-subtrie? tr key))
(defn trace_subkeys [tr] (trie-keys tr))
(defn lookup [tr addr]
  (subtrace-at tr addr))  ; e[e]

(defn make_env [parent]
  (cons (ref {}) parent))

(defn match_bind [pattern inputs env]
  (dosync
   (letfn [(mb [pattern inputs]
             (if (not (seq? pattern))
               (ref-set (first env) pattern inputs)
               (if (not (empty? pattern))
                 (do (mb (first pattern) (first inputs))
                     (mb (rest pattern) (rest inputs))))))]
     (mb pattern inputs))
   env))

(defn env_lookup [env name]
  (assert (not (empty? env)))
  (or (get (deref (first env)) name)
      (env_lookup (rest env) name)))

(defn interpret_prim [executable inputs intervention-trace]
  ;; Not sure what this is supposed to do
  0)

(defn pprint-noncolliding [x]
  ;; x is a trie.  need to prettyprint it somehow.
  0)

(defn error [x]
  (assert (string? x))
  (Exception. x))

; Other builtins

(defn flip [weight] (<= (rand) weight))

(defn uniform [a b] (+ (rand (- b a)) a))

(defn capture_tag_address [& stuff]
  stuff)

(defn resolve_tag_address [stuff]
  stuff)

; Metaprob arrays (node with children 0 ... size-1)

(defn mp*array-from-seq [sequ]
  (trie-from-map (zipmap (range (count sequ))
                         (for [member sequ] (new-trie member)))))

; Size of a metaprob array

(defn mp*size [trie]
  (letfn [(size-from [trie i]
            (if (has-subtrie? trie i)
              (+ 1 (size-from trie (+ i 1)))
              0))]
    (size-from trie 0)))

(defn seq-from-mp*array [mp*array]
  (for [i (range (mp*size mp*array))]
    (value-at mp*array [i])))

; Convert metaprob array to metaprob list ?
(defn array-to-list [a] 0)

; Translated from prelude.vnts.

; Constructors for metaprob-list type

(def mp*nil (new-trie 'nil))

; Predicates for metaprob-list type

(defn mp*null? [thing]
  ;; (= mp*list mp*nil)  ??
  (and (trie? thing)
       (has-value? thing)
       (= (value thing) 'nil)
       (not (has-subtrie? thing :rest))))

(defn mp*pair? [thing]
  (and (trie? thing)
       (has-subtrie? thing :rest)))

(defn mp*list? [thing]
  (and (trie? thing)
       (or (has-subtrie? thing :rest)
           (= (value thing) 'nil))))

; Selectors

(defn mp*first [mp*list]
  (value mp*list))

(defn mp*rest [mp*list]
  (subtrie mp*list :rest))

; Higher level operators

(defn mp*list-to-clojure-seq [s]
  (if (mp*null? s)
    []
    (let [[hd tl] s]
      (conj (mp*list-to-clojure-seq tl) hd))))

(defn mp*seq-to-clojure-seq [s]
  (if (trie? s)
    (if (mp*list? s)
      (mp*list-to-clojure-seq s)
      (for [i (range (mp*size s))]
        (value-at s [i])))
    s))

(declare mp*map)

(defn mp*apply [mp*fn mp*seq]
  (apply mp*fn (mp*seq-to-clojure-seq mp*seq)))

(defn mp*list-map [mp*fn mp*list]
  (if (mp*null? mp*list)
    []
    (pair (mp*apply mp*fn (mp*first mp*list))
          (mp*map mp*fn (mp*rest mp*list)))))

(defn mp*array-map [mp*fn mp*array]
  (let [n (mp*size mp*array)
        r (range n)]
    (trie-from-map (zipmap r
                           (for [i r] (mp*apply mp*fn (value-at mp*array [i])))))))

(defn mp*map [mp*fn thing]
  (if (trie? thing)
    (if (mp*list? thing)
      (mp*list-map mp*fn thing)
      (mp*array-map mp*fn thing))
    (for [x thing] (mp*apply mp*fn x))))



(defn name-for-definiens [pattern]
  (if (symbol? pattern)
    (if (= pattern '_)
      'definiens
      pattern)
    'definiens))
