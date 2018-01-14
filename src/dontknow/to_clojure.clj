(ns dontknow.to-clojure
  (:require [dontknow.trie :refer :all])
  (:require [clojure.pprint :as pp]))

; Read a source code trie (i.e. trace) in ("a-value" "prop" "val") form
; and translate it into Clojure.

; In emacs you will want this:
;  (put 'program 'clojure-indent-function ':defn)

; A 'form' is an expression or a definition or a block of forms.

; *using-program?* - if true: generate (program ...), if false: generate (fn ...)

(def ^:dynamic *using-program?* true)

(declare to-clojure)
(declare subexpression-to-clojure)

(defn to-list [x]
  (if (list? x) x (concat (list) x)))

(defn qons [x y]
  (if (list? y)
    (conj y x)
    (conj (concat (list) y) x)))

; tr is a trie / trace.

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

(defn definition-pattern [tr] (subtrie tr "pattern"))

(defn definition-name [tr]
  (let [pattern (definition-pattern tr)]    ;trie
    (if (and (has-value? pattern)
             (= (value pattern) "variable"))
      (let [name (get-value pattern "name")]
        (if (= name "_")
          "definiens"
          name))
      "definiens")))

(defn definition-rhs [tr]
  (subtrie tr (definition-name tr)))

(def colliding-names
  #{"first" "rest" "last" "range" "pprint" "map" "replicate"
    "program" "block"})

(defn to-symbol [strng]
  (symbol (if (contains? colliding-names strng)
            (str strng '-noncolliding)
            strng)))

(defn program-definition? [tr]
  (and (definition? tr)
       (= (value (definition-rhs tr)) "program")))

(declare form-to-clojure)

; Convert a metaprob expression (not a definition) to clojure

(defn expr-to-clojure [tr]
  (form-to-clojure tr false))

; Allow generation of (program [x] (foo) (bar)) instead of
; (program [x] (block (foo) (bar)))

(defn form-to-formlist [form]
  (if (and (list? form)
           (= (first form) 'block)
           (>= (count form) 3))
    (rest form)
    (list form)))

(defn pattern-to-pattern [tr]
  (case (value tr)
    "variable" (to-symbol (get-value tr "name"))
    "tuple" (vec (map pattern-to-pattern
                      (sequential-subtries tr)))
    (do 
      (print ["invalid pattern" (value tr)]) (newline)
      (list "invalid pattern" (value tr)))))

; Top-level definition.  Formerly this condensed def + fn to defn, but
; that doesn't work if we're going to support metaprob reification - 
; we always need def + program.

(defn definition-to-clojure [tr]
  (list 'def
        (pattern-to-pattern (definition-pattern tr))
        (expr-to-clojure (definition-rhs tr))))

; Returns a single clojure expression

(defn formlist-to-form [forms]
  (assert (seq? forms))
  (if (empty? forms)
    'nil
    (if (empty? (rest forms))
      (first forms)
      (qons 'block forms))))

; VKM has requested (trace_set (lookup ...) ...)
; i.e. trace_set maybe a macro, lookup is lvalue-like

(defn peep [app]
  (if (= (first app) 'trace_set)
    (let [arg1 (first (rest app))]
      ;; Look for (trace_set (lookup t arg1) v)
      (if (and (seq? arg1)
               (do (assert (= (first arg1) 'lookup)
                           "Troublesome trace_set")
                   (print (format "Peep: %s %s \n" (first app) arg1))
                   (= (first arg1) 'lookup)))
        (qons 'trace_set_at (concat (rest arg1) (rest (rest app))))
        app))
    app))

(defn subexpressions-to-clojure [tr]
  (to-list
   (map expr-to-clojure
        (sequential-subtries tr))))

(defn block-to-clojure [trs]
  (if (empty? trs)
    'nil
    (if (empty? (rest trs))
      (expr-to-clojure (first trs))
      (qons 'block
            (letfn [(foo [trs]
                      (if (empty? (rest trs))
                        (qons (expr-to-clojure (first trs)) nil)
                        (qons (to-clojure (first trs))
                              (foo (rest trs)))))]
              (foo trs))))))

(defn to-clojure [tr]
  (form-to-clojure tr true))

(defn form-to-clojure [tr def-ok?]
  (let [tr (if (trie? tr) tr (new-trie tr))]
    (case (value tr)
      "application" (to-list
                     (subexpressions-to-clojure tr))
      "variable" (to-symbol (get-value tr "name"))
      "literal" (get-value tr "value")
      "program" (qons (if *using-program?* 'program 'fn)
                      (qons (pattern-to-pattern (subtrie tr "pattern"))
                            (form-to-formlist
                             (subexpression-to-clojure tr "body"))))
      "if" (list 'if
                 (subexpression-to-clojure tr "predicate")
                 (subexpression-to-clojure tr "then")
                 (subexpression-to-clojure tr "else"))
      "block" (block-to-clojure (sequential-subtries tr))
      "splice" (list 'splice
                     (subexpression-to-clojure tr "expression"))
      "this" 'this
      "tuple" (vec (subexpressions-to-clojure tr))
      "unquote" (list 'unquote
                      (subexpression-to-clojure tr "expression"))
      "with_address" (list 'with-address
                           ;; Not directly executable in clojure
                           (subexpression-to-clojure tr "tag")
                           (subexpression-to-clojure tr "expression"))
      "definition" (if def-ok?
                     (definition-to-clojure tr)
                     (do (print (format "** definition not allowed here: ~s\n"
                                        (definition-name tr)))
                         ''definition-not-allowed-here))
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

; Returns a list of clojure expressions

(defn top-level-to-clojure [tr]
  (if (= (value tr) "block")
    (let [subs (sequential-subtries tr)]
      (to-list
         (concat (declarations subs)
                 (map to-clojure subs))))
    (qons (to-clojure tr) nil)))

(defn subexpression-to-clojure [tr key]
  (let [sub (subtrie tr key)]
    (assert (trie? sub) (list "missing" key))
    (expr-to-clojure sub)))

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

; exprs is list of clojure expressions, as returned by top-level-to-clojure

(defn write-to-file [exprs outpath]
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
      (doseq [form exprs]
        ;; Add forward declarations??
        (write-one-form form)))))

(defn convert [inpath outpath]
  (write-to-file (top-level-to-clojure (reconstruct-trace (read-from-file inpath)))
                 outpath))

