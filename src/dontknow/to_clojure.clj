(ns dontknow.to-clojure
  (:require [dontknow.trie :refer :all]))

; Read a source code trie (i.e. trace) in ("a-value" "prop" "val") form.
; tr is a trie.

(declare to-clojure)
(declare subexpression-to-clojure)

(defn qons [x y] (conj y x))

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
  #{"first" "rest" "last" "range" "pprint" "map"})

(defn to-symbol [strng]
  (symbol (if (contains? colliding-names strng)
            (str "mp-" strng)
            strng)))

(defn definition-to-clojure [tr]
  (let [name (definition-name tr)]
    (list 'def
          (to-symbol name)
          (subexpression-to-clojure tr name))))

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

(defn program-definition? [tr]
  (and (definition? tr)
       (let [rhs (subtrie tr (definition-name tr))]
         (= (value rhs) "program"))))

(defn form-to-formlist [form]
  (if (and (list? form) (= (first form) 'do))
    (rest form)
    (list form)))

(defn formlist-to-form [forms]
  (assert list? forms)
  (if (empty? forms)
    'nil
    (if (empty? (rest forms))
      (first forms)
      (qons 'do forms))))

; list of trie -> list of clojure form

(defn block-to-body [trs]
  (if (empty? trs)
    '()
    (let [more (block-to-body (rest trs))]    ; list of forms
      (if (definition? (first trs))
        (let [name (definition-name (first trs))
              rhs (subtrie (first trs) name)]
          ;; A definition must not be last expression in a block
          (assert (not (empty? (rest trs))))
          (if (program-definition? (first trs))
            (let [pattern (to-clojure (subtrie rhs "pattern"))
                  prog-body (form-to-formlist (to-clojure (subtrie rhs "body")))
                  spec (qons (pattern-to-pattern (subtrie (first trs) "pattern"))
                             (qons pattern prog-body))
                  more1 (first more)]
              (if (and (list? more1)
                       (= (first more1) 'letfn))
                (do (assert empty? (rest more))
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
                (do (assert empty? (rest more))
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

(defn to-clojure [tr]
  (let [tr (if (trie? tr) tr (new-trie tr))]
    (case (value tr)
      "application" (for [i (range (trie-count tr))]
                      (subexpression-to-clojure tr i))
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
      "definition" (definition-to-clojure tr)    ;ASSIGNMENT
      "splice" (list 'splice
                     (subexpression-to-clojure tr "expression"))
      "this" 'this
      "tuple" (vec (for [i (range (trie-count tr))]
                     (subexpression-to-clojure tr i)))
      "unquote" (list 'unquote
                      (subexpression-to-clojure tr "expression"))
      (list "unrecognized expression type" (value tr)))))

; Convert the top-level expression in a file to top-level
; clojure (using def for definitions).

(defn top-level-to-clojure [tr]
  (if (= (value tr) "block")
    (formlist-to-form
          (for [i (range (trie-count tr))]
            (let [sub (subtrie tr i)]
              (if (definition? sub)
                (definition-to-clojure sub)
                (to-clojure sub)))))
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
              (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch
                (clojure.pprint/write form :pretty true :stream w))
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

