(ns dontknow.to-clojure
  (:require [dontknow.trie :refer :all])
  (:require [dontknow.library :refer :all])
  (:require [clojure.pprint :as pp]))

; Read a source code trie (i.e. trace) in ("a-value" "prop" "val") form
; and translate it into Clojure.

; In emacs you will want this:
;  (put 'program 'clojure-indent-function ':defn)
;  (put 'define 'clojure-indent-function ':defn)

; A 'form' is an expression or a definition or a block of forms.

; *using-program?* - if true: generate (program ...), if false: generate (fn ...)

(def ^:dynamic *using-program?* true)

(declare to-clojure)
(declare subexpression-to-clojure)

(defn to-list [x]
  ;; Surely there has to be a better way to do this
  (if (list? x)
    x
    (let [z (apply list x)]
      (assert (list? z) "qons malfunction")
      z)))

(defn qons [x y]
  (to-list (conj y x)))

; tr is a trie / trace.

(defn subvalue [tr key]
  (value (subtrie tr key)))

; Definitions

(defn definition? [tr]
  (and (trie? tr)
       (has-value? tr)
       (= (value tr) "definition")))

(defn definition-pattern [tr] (subtrie tr "pattern"))

(defn definition-name [tr]
  (let [pattern (definition-pattern tr)]    ;trie
    (if (and (has-value? pattern)
             (= (value pattern) "variable"))
      (let [name (subvalue pattern "name")]
        (if (= name "_")
          "definiens"
          name))
      "definiens")))

(defn definition-rhs [tr]
  (subtrie tr (definition-name tr)))

(defn program-definition? [tr]
  (and (definition? tr)
       (= (value (definition-pattern tr)) "variable")
       (= (value (definition-rhs tr)) "program")))

; Metaprob code sometimes has local variables that collide with
; important special forms / macros.

(def colliding-names
  #{;; "first" "rest" "last" "range" "pprint" "map" "replicate"
    "program" "block"})

(defn to-symbol [strng]
  (symbol (if (contains? colliding-names strng)
            (str strng '-noncolliding)
            strng)))

(defn ensure-list [z noise]
  (assert (not (trie? z))
          ["answer should be an s-expression" noise z])
  (assert (if (seq? z) (list? z) true)
          ["answer should be a list" noise z])
  z)


; -----------------------------------------------------------------------------
; Convert a metaprob expression (not a definition) to clojure

(declare form-to-clojure)

(defn expr-to-clojure [tr]
  (form-to-clojure tr false))

; Allow generation of (program [x] (foo) (bar)) instead of
; (program [x] (block (foo) (bar)))

(defn form-to-formlist [form]
  (if (and (seq? form)
           (= (first form) 'block)
           (not (= (count form) 2)))
    (rest form)
    (list form)))

; Convert a metaprob pattern, used in a definition pat = val, to
; a clojure form that can be used with the define macro

(defn pattern-to-pattern [tr]
  (case (value tr)
    "variable" (to-symbol (subvalue tr "name"))
    "tuple" (vec (map pattern-to-pattern
                      (subtries-to-seq tr)))
    (do 
      (print ["invalid pattern" (value tr)]) (newline)
      (list "invalid pattern" (value tr)))))

; Top-level definition.  Formerly, this condensed def + fn to defn, but
; that doesn't work if we're going to support metaprob reification - 
; we always need def + program.

(defn definition-to-clojure [tr]
  (list 'define
        (pattern-to-pattern (definition-pattern tr))
        (expr-to-clojure (definition-rhs tr))))

(defn subexpressions-to-clojure [tr]
  (map expr-to-clojure
       (subtries-to-seq tr)))

(defn block-to-clojure-1 [trs def-ok?]
  (if (empty? trs)
    (qons 'block nil)
    (qons 'block
          (letfn [(dive [trs]
                    (if (empty? (rest trs))
                      (qons (form-to-clojure (first trs) def-ok?) nil)
                      (qons (to-clojure (first trs))
                            (dive (rest trs)))))]
            (dive trs)))))

(defn block-to-clojure [trs def-ok?]
  (ensure-list (block-to-clojure-1 trs def-ok?)
               "block"))

; Convert a top level form (expression or definition)

(defn to-clojure [tr]
  (form-to-clojure tr true))

(defn form-to-clojure-1 [tr def-ok?]
  (let [tr (if (trie? tr) tr (new-trie tr))]
    (case (value tr)
      "application" (to-list
                     (subexpressions-to-clojure tr))
      "variable" (to-symbol (subvalue tr "name"))
      "literal" (subvalue tr "value")
      "program" (qons 'program
                      (qons (pattern-to-pattern (subtrie tr "pattern"))
                            ;; For readability, allow x y instead of (block x y)
                            (form-to-formlist
                             (ensure-list
                             (subexpression-to-clojure tr "body") "blah 2"))))
      "if" (list 'if
                 (subexpression-to-clojure tr "predicate")
                 (subexpression-to-clojure tr "then")
                 (subexpression-to-clojure tr "else"))
      "block" (ensure-list
              (block-to-clojure (subtries-to-seq tr) def-ok?) "blah")
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
      "definition" (do (if (not def-ok?)
                         (print (format "** definition not allowed here: %s\n"
                                        (definition-pattern tr))))
                       (definition-to-clojure tr))
      (list "unrecognized expression type" (value tr)))))

(defn form-to-clojure [tr def-ok?]
  (ensure-list (form-to-clojure-1 tr def-ok?) ["form-to-clojure" (value tr)]))

(defn declarations [subs]
  (assert (seq? subs))
  (let [defs (rest (filter program-definition? subs))]
    (do (assert (seq? defs) "no brainer")
    (if (empty? defs)
      (list)
      (list (qons 'declare (for [d defs]
                             ;; This is wrong - we need to collect all
                             ;; the variables in the pattern.
                             (to-symbol (definition-name d)))))))))

; Returns a list of clojure expressions

(defn top-level-to-clojure [tr]
  (if (= (value tr) "block")
    (let [subs (subtries-to-seq tr)]
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
  (if (seq? form)
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
             '(:refer-clojure)       ; Don't import clojure core!
             '(:require [dontknow.builtin :refer :all])))
      (doseq [form exprs]
        ;; Add forward declarations??
        (write-one-form form)))))

(defn convert [inpath outpath]
  (write-to-file (top-level-to-clojure (reconstruct-trace (read-from-file inpath)))
                 outpath))

