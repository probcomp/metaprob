(ns metaprob.to-clojure
  (:require [metaprob.trace :refer :all])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin
             :as b
             :exclude [not
                       assert
                       pprint
                       list
                       and
                       or
                       first
                       rest
                       last
                       nth
                       range]])
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as cs])
  (:require [clojure.java.io :as io]))

; Read a source code trie (i.e. trace) in ("a-value" "prop" "val") form
; and translate it into Clojure.

; In emacs you will want this:
;  (put 'program 'clojure-indent-function ':defn)
;  (put 'define 'clojure-indent-function ':defn)

; A 'form' is an expression or a definition or a block of forms.

; *using-program?* - if true: generate (program ...), if false: generate (fn ...)

; `nest` is a dict.  Fields:
;   :top   - true iff at top level in a position that's OK for a define
;   :bare  - true iff at top level, no local vars in scope

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
  (value (subtrace tr key)))

; Definitions

(defn definition? [tr]
  (and (trie? tr)
       (has-value? tr)
       (= (value tr) "definition")))

(defn definition-pattern [tr] (subtrace tr "pattern"))

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
  (subtrace tr (definition-name tr)))

(defn program-definition? [tr]
  (and (definition? tr)
       (= (value (definition-pattern tr)) "variable")
       (= (value (definition-rhs tr)) "program")))

; Metaprob code sometimes has local variables that collide with
; important special forms / macros.

(def colliding-names
  #{;; "first" "rest" "last" "range" "pprint" "map" "replicate"
    "program" "block" "define"})

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

(defn expr-to-clojure [tr nest]
  (form-to-clojure tr (assoc nest :top false)))

;; Trace       => clojure                   => trace
;; (x)->{a;b;} => (program [x] (block a b)) => (x)->{a;b;}
;; (x)->{a;}   => (program [x] (block a))   => (x)->{a;}
;; (x)->a      => (program [x] a)           => (x)->a
;;
;; safe abbreviation:
;; (x)->{a;b;} => (program [x] a b) => (x)->{a;b;}

; Allow generation of (program [x] (foo) (bar)) instead of
; (program [x] (block (foo) (bar)))

(def elide-block-sometimes true) ;screws up trace numbering

(defn form-to-formlist [form]
  (if (and elide-block-sometimes
           (seq? form)
           (= (name (first form)) "block")
           (not (= (count form) 2)))
    (rest form)
    (list form)))

; Convert a metaprob pattern, used in a definition pat = val, to
; a clojure form that can be used with the define macro

(defn pattern-to-pattern [tr]
  (case (value tr)
    "variable" (to-symbol (subvalue tr "name"))
    "tuple" (vec (map pattern-to-pattern
                      (subtraces-to-seq tr)))
    (do 
      (print ["invalid pattern" (value tr)]) (newline)
      (list "invalid pattern" (value tr)))))

; Top-level definition.  Formerly, this condensed def + fn to defn, but
; that doesn't work if we're going to support metaprob reification - 
; we always need def + program.

(defn definition-to-clojure [tr nest]
  (list 'define
        (pattern-to-pattern (definition-pattern tr))
        (expr-to-clojure (definition-rhs tr) nest)))

(defn subexpressions-to-clojure [tr nest]
  (map (fn [z] (expr-to-clojure z nest))
       (subtraces-to-seq tr)))

(defn block-to-clojure-1 [trs nest]
  (if (empty? trs)
    (qons 'block nil)
    (qons 'block
          (letfn [(dive [trs]
                    (if (empty? (rest trs))
                      (qons (form-to-clojure (first trs) nest) nil)
                      (qons (to-clojure (first trs) nest)
                            (dive (rest trs)))))]
            (dive trs)))))

(defn block-to-clojure [trs nest]
  (ensure-list (block-to-clojure-1 trs nest)
               "block"))

(defn program-to-clojure [pat-trace body-trace nest]
  ;; For readability, allow x y instead of (block x y)
  (let [nest (if (b/empty-trace? pat-trace)
               nest
               (assoc nest :bare false))
        body (form-to-formlist (to-clojure body-trace nest))
        form (qons 'probprog
                   (qons (pattern-to-pattern pat-trace)
                         body))]
    (if (get nest :bare)
      form
      ;; The environment isn't captured, so it's not possible to 
      ;; interpret the source code.
      (qons 'export-probprog
            (qons form nil)))))

(defn with-address-to-clojure [tr nest]
  (list 'with-addr
        ;; Not directly executable in clojure
        (subexpression-to-clojure tr "tag" nest)
        (subexpression-to-clojure tr "expression" nest)))


;; Convert a top level form (expression or definition)

(defn to-clojure
  ([tr nest]
   (form-to-clojure tr nest))
  ([tr]
   (form-to-clojure tr {:top true :bare true})))

(defn form-to-clojure-1 [tr nest]
  (let [tr (if (trie? tr) tr (new-trace tr))]
    (case (value tr)
      "application" (let [form (subexpressions-to-clojure tr nest)]
                      (to-list (if (= (first form) 'mk_nil)
                                 (cons 'empty-trace (rest form))
                                 form)))
      "variable" (to-symbol (subvalue tr "name"))
      "literal" (subvalue tr "value")
      "program" (program-to-clojure (subtrace tr "pattern")
                                    (subtrace tr "body")
                                    nest)
      "if" (list 'if
                 (subexpression-to-clojure tr "predicate" nest)
                 (subexpression-to-clojure tr "then" nest)
                 (subexpression-to-clojure tr "else" nest))
      "block" (ensure-list
               (block-to-clojure (subtraces-to-seq tr) nest)
               "blah")
      "splice" (list 'mp-splice
                     (subexpression-to-clojure tr "expression" nest))
      "this" '(&this)
      "tuple" (qons 'tuple (subexpressions-to-clojure tr nest))
      "unquote" (list 'ml-unquote
                      (subexpression-to-clojure tr "expression" nest))
      "with_address" (with-address-to-clojure tr)
      "with-addr" (with-address-to-clojure tr)
      "definition" (do (if (not (get nest :top))
                         (print (format "** definition not allowed here: %s\n"
                                        (definition-pattern tr))))
                       (definition-to-clojure tr nest))
      (list "unrecognized expression type" (value tr)))))

(defn form-to-clojure [tr nest]
  (ensure-list (form-to-clojure-1 tr nest) ["form-to-clojure" (value tr)]))

;; Get list of names for clojure (declare ...) at top of generated file

(defn declarations [subs]
  (assert (seq? subs))
  (let [defs (filter program-definition? subs)]
    (do (assert (seq? defs) "no brainer")
    (if (empty? defs)
      (list)
      (list (qons 'declare (for [d defs]
                             ;; This is wrong - we need to collect all
                             ;; the variables in the pattern.
                             (to-symbol (definition-name d)))))))))

;; Returns a list of clojure expressions.
;; These are at the top level of the file, so not subject to constraints on trace path preservation.

(defn top-level-to-clojure [tr]
  (let [nest {:top true :bare true}]
    (if (= (value tr) "block")
      (let [subs (subtraces-to-seq tr)]
        (to-list
         (concat (declarations subs)
                 (map (fn [sub] (to-clojure sub nest))
                      subs))))
      (qons (to-clojure tr nest) nil))))

(defn subexpression-to-clojure [tr key nest]
  (let [sub (subtrace tr key)]
    (assert (trie? sub) ["missing field in parse tree" key])
    (expr-to-clojure sub nest)))

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
            (trace-from-map m)
            (trace-from-map m val)))))
    (list "[not a trace!?]" form)))

; My this is painful.

(defn read-from-file [inpath]
  (binding [*read-eval* false]
    (with-open [r (java.io.PushbackReader.
                   (clojure.java.io/reader
                    (clojure.java.io/input-stream inpath)))]
      (read r))))

;; exprs is list of clojure expressions, as returned by top-level-to-clojure
;; We tickle a clojure pprint bug if the :only list is []
;; https://dev.clojure.org/jira/browse/CLJCLR-97

(def metaprob-inherits-from-clojure '[ns declare])

(defn get-requirements [ns-name]
  (filter (fn [x]
            (not (= (name (first x)) (name ns-name))))
          '([metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all])))

(defn write-to-file [forms this-ns outpath]
  ;; (print (format "** ns A = %s\n" *ns*))(flush)
  (let [nsname (ns-name this-ns)]
    (with-open [w (clojure.java.io/writer
                   (clojure.java.io/output-stream outpath))]
      (letfn [(write-one-form [form]
                (pp/with-pprint-dispatch pp/code-dispatch
                  (pp/write form :pretty true :stream w))
                (binding [*out* w]
                  (println)
                  (println)))]
        (binding [*out* w]
          (print ";; This file was automatically generated\n\n"))
        ;; *ns* at this point is assumed to be a normal clojure
        ;; namespace, the same one in which nsname was created
        (write-one-form
         `(~'ns ~nsname
            (:refer-clojure :only ~metaprob-inherits-from-clojure)
            (:require ~@(get-requirements nsname))))
        (doseq [form forms]
          (binding [*ns* this-ns]
            (write-one-form form)))))))

;; Damn, this is no good.  No idea how many terminal path components
;; to include in the dot-separated namespace name.
;; - so this code is unused for now... keeping it because sunk cost.

(defn path-to-namespace-name [path]
  (let [name (.getName (io/file path))
        dot (cs/last-index-of name ".")]
    (cs/replace
     (if dot
       (subs name 0 dot)
       name)
     "_"
     "-")))

(defn create-namespace [nsname]
  (let [the-ns (create-ns (symbol nsname))]
    (binding [*ns* the-ns]
      (refer 'clojure.core :only metaprob-inherits-from-clojure)
      (doseq [r (get-requirements nsname)]
        (require r)))
    the-ns))

;; reconstruct-trace is sensitive to *ns*.  Set *ns* to something
;; approximating what will be in effect when the file is eventually
;; compiled and loaded.  I don't think the trace
;; is sensitive, but it might be, so take care in reading it.

(defn read-trace-from-file [inpath ns]
  (binding [*ns* ns]
    (reconstruct-trace (read-from-file inpath))))

(defn convert [inpath outpath nsname]
  (let [ns (create-namespace nsname)]
    (write-to-file (top-level-to-clojure (read-trace-from-file inpath ns))
                   ns
                   outpath)))
