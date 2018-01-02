(ns dontknow.to-clojure
  (:require [dontknow.trie :refer :all])
  (:require [clojure.pprint :as pp]))

; Read a source code trie (i.e. trace) in ("a-value" "prop" "val") form.
; tr is a trie.

(declare to-clojure)
(declare subexpression-to-clojure)

(defn to-list [x]
  (if (list? x) x (concat (list) x)))

(defn qons [x y]
  (if (list? y)
    (conj y x)
    (conj (concat (list) y) x)))

(defn get-value [tr key]
  (value (subtrie tr key)))

(defn sequential-subtries [tr]
  (for [i (range (trie-count tr))]
    (subtrie tr i)))

; cf. metaprob/src/propose.py

(defn definition? [tr]
  (and (trie? tr)
       (has-value? tr)
       (= (value tr) "definition")))

(defn definition-name [tr]
  (let [pattern (subtrie tr "pattern")]    ;trie
    (if (and (has-value? pattern)
             (= (value pattern) "variable"))
      (let [name (get-value pattern "name")]
        (if (= name "_")
          "definiens"
          name))
      "definiens")))

(def colliding-names
  #{"first" "rest" "last" "range" "pprint" "map" "replicate"})

(defn to-symbol [strng]
  (symbol (if (contains? colliding-names strng)
            (str "mp-" strng)
            strng)))

(defn program-definition? [tr]
  (and (definition? tr)
       (let [rhs (subtrie tr (definition-name tr))]
         (= (value rhs) "program"))))

(declare form-to-formlist pattern-to-pattern)

; Returns (name pattern . body), for use with (defn ...) or (letfn [...] ...)
(defn process-definition [tr]
  (assert program-definition? tr)
  (let [name (definition-name tr)
        rhs (subtrie tr name)           ;a program-expression
        prog-body (form-to-formlist (to-clojure (subtrie rhs "body")))]
    (qons (to-symbol name)
          (qons (pattern-to-pattern (subtrie rhs "pattern"))
                prog-body))))

; Top-level definition
(defn definition-to-clojure [tr]
  (if (program-definition? tr)
    (qons 'defn
          (process-definition tr))
    (let [name (definition-name tr)
          rhs (subtrie tr name)]
      (list 'def
            (to-symbol name)
            (to-clojure rhs)))))

(defn pattern-to-pattern [tr]
  (case (value tr)
    "variable" (to-symbol (get-value tr "name"))
    "tuple" (vec (for [sub (sequential-subtries tr)]
                   (pattern-to-pattern sub)))
    (do 
      (print ["invalid pattern" (value tr)]) (newline)
      (list "invalid pattern" (value tr)))))

; list of trie -> list of clojure forms

(defn body-to-clojure [trs]
  (if (empty? trs)
      '()
      ; (if (definition? (first trs))
      ;   (if (program? (subtrie trs ...))
      ;     ...
      (qons (to-clojure (first trs))
            (body-to-clojure (rest trs)))))

(defn form-to-formlist [form]
  (if (and (list? form) (= (first form) 'do))
    (rest form)
    (list form)))

(defn formlist-to-form [forms]
  (assert (seq? forms))
  (if (empty? forms)
    'nil
    (if (empty? (rest forms))
      (first forms)
      (if (list? forms)
        (qons 'do forms)
        (qons 'do (concat (list) forms))))))

; list of trie -> list of clojure form

(defn block-to-body [trs]
  (if (empty? trs)
    '()
    (let [more (block-to-body (rest trs))]    ; list of forms
      (if (definition? (first trs))
        (let [name (definition-name (first trs))
              rhs (subtrie (first trs) name)]
          ;; A definition must not be last expression in a block
          (assert (not (empty? (rest trs)))
                  "Definition occurs at end of block")
          (if (program-definition? (first trs))
            (let [spec (process-definition (first trs))
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
            (let [sym (pattern-to-pattern (subtrie (first trs) "pattern"))
                  more1 (first more)]
              (if (and (list? more1)
                       (= (first more1) 'let))
                ;; Combine two lets into one
                (do (assert (empty? (rest more)))
                    (let [[_ specs & body] more1]
                      (list             ;Single form
                       (qons 'let
                             (qons (vec (cons sym (cons (to-clojure rhs) specs)))
                                   body)))))
                (list                   ;Single form
                 (qons 'let
                       (qons [sym (to-clojure rhs)]
                             more)))))))
        ;; Not a definition
        (qons (to-clojure (first trs)) more)))))

(defn block-to-clojure [trs]
  (formlist-to-form (block-to-body trs)))

(defn peep [app]
  (let [arg1 (first (rest app))]
    ;; Look for (trace_set (lookup t arg1) v)
    (if (and (= (first app) 'trace_set)
             (seq? arg1)
             (do (assert (= (first arg1) 'lookup)
                         "Troublesome trace_set")
                 (print (format "Peep: %s %s \n" (first app) arg1))
                 (= (first arg1) 'lookup)))
      (qons 'trace_set_at (concat (rest arg1) (rest (rest app))))
      app)))

(defn to-clojure [tr]
  (let [tr (if (trie? tr) tr (new-trie tr))]
    (case (value tr)
      "application" (peep
                     (to-list
                      (for [i (range (trie-count tr))]
                        (subexpression-to-clojure tr i))))
      "variable" (to-symbol (get-value tr "name"))
      "literal" (get-value tr "value")
      "program" (list 'fn
                      (pattern-to-pattern (subtrie tr "pattern"))
                      (subexpression-to-clojure tr "body"))
      "if" (list 'if
                 (subexpression-to-clojure tr "predicate")
                 (subexpression-to-clojure tr "then")
                 (subexpression-to-clojure tr "else"))
      "block" (block-to-clojure (for [i (range (trie-count tr))] (subtrie tr i)))
      "definition" (definition-to-clojure tr)
      "splice" (list 'splice
                     (subexpression-to-clojure tr "expression"))
      "this" 'this
      "tuple" (vec (for [i (range (trie-count tr))]
                     (subexpression-to-clojure tr i)))
      "unquote" (list 'unquote
                      (subexpression-to-clojure tr "expression"))
      "with_address" (list 'with-address
                           ;; Not directly executable in clojure
                           (subexpression-to-clojure tr "tag")
                           (subexpression-to-clojure tr "expression"))
      (list "unrecognized expression type" (value tr)))))

; Convert the top-level expression in a file to top-level
; clojure (using def for definitions).

(defn declarations [subs]
  (assert (seq? subs))
  (let [defs (rest (filter program-definition? subs))]
    (do (assert (seq? defs) "no brainer")
    (if (empty? defs)
      (list)
      (list (qons 'declare (for [d defs]
                             (to-symbol (definition-name d)))))))))

(defn top-level-to-clojure [tr]
  (if (= (value tr) "block")
    (let [subs (sequential-subtries tr)]
      (formlist-to-form
         (concat (declarations subs)
                 (for [sub subs]
                   (if (definition? sub)
                     (definition-to-clojure sub)
                     (to-clojure sub))))))
    (to-clojure tr)))

(defn subexpression-to-clojure [tr key]
  (let [sub (subtrie tr key)]
    (assert (trie? sub) (list "missing" key))
    (to-clojure sub)))

; Create a trie from a file containing a representation of a
; trie (written by the python script).

(defn reconstruct-trace [form]
  (if (list? form)
    (let [val (first form)]
      (letfn [(mapify [things]
                (if (empty? things)
                  (hash-map)
                  (assoc (mapify (rest (rest things)))
                         (first things)
                         (reconstruct-trace (first (rest things))))))]
        (let [m (mapify (rest form))]
          (if (= val :none)
            (trie-from-map m)
            (trie-from-map m val)))))
    (list "[not a trace!?]" form)))

; My this is painful.

(defn read-from-file [inpath]
  (binding [*read-eval* false]
    (with-open [r (java.io.PushbackReader.
                   (clojure.java.io/reader
                    (clojure.java.io/input-stream inpath)))]
      (read r))))

(defn write-to-file [obj outpath]
  (with-open [w (clojure.java.io/writer
                 (clojure.java.io/output-stream outpath))]
    (letfn [(write-one-form [form]
              (pp/with-pprint-dispatch pp/code-dispatch
                (pp/write form :pretty true :stream w))
              (binding [*out* w]
                (println)
                (println)))]
      (write-one-form
       (list 'ns
             'dontknow.metaprob
             '(:require [dontknow.builtin :refer :all])))
      (if (= (first obj) 'do)
        (doseq [form (rest obj)]
          ; Add forward declarations??
          (write-one-form form))
        (write-one-form obj)))))

(defn convert [inpath outpath]
  (write-to-file (top-level-to-clojure (reconstruct-trace (read-from-file inpath))) outpath))

