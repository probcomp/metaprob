
; Read a source code trie / trace in {:v 3 "x" 7} form, and

; tr is a map (hashtable)

(declare subexpression-to-clojure)

(defn get-value [tr]
  (if (map? tr) (get tr :v) tr))

; cf. metaprob/src/propose.py

(defn name-for-definiens [tr]
  (if (and (map? tr)
           (= (get tr :v) "variable")
           (not (= (get tr :v) "_")))
    (get-value (get tr "name"))
    'definiens))

(defn definition-to-clojure [tr]
  (let [pattern (get tr "pattern")
        name (name-for-definiens pattern)]
    (list 'def
          (symbol name)
          (subexpression-to-clojure tr name))))

; Ignore this
(defn definition-to-clojure-loser [tr]
  (let [vars (remove (fn [x] (= x :v)) (keys tr))]
    (concat (rest (for [var vars]
                    (list 'declare var)))
            (for [var vars]
              (let [d (get tr var)]
                (list 'def
                      var
                      (subexpression-to-clojure d var)))))))

(defn to-clojure [tr]
  (let [tr (if (map? tr) tr {:v tr})]
    (case (get tr :v)
      "application" (for [i (range (- (count tr) 1))]
                      (subexpression-to-clojure tr i))
      "variable" (symbol (get-value (get tr "name")))
      "literal" (get-value (get tr "value"))
      "program" (list 'fn
                      (subexpression-to-clojure tr "pattern")
                      (subexpression-to-clojure tr "body"))
      "if" (list 'if
                 (subexpression-to-clojure tr "predicate")
                 (subexpression-to-clojure tr "then")
                 (subexpression-to-clojure tr "else"))
      "block" (cons 'do
                    (for [i (range (- (count tr) 1))]
                      (subexpression-to-clojure tr i)))
      "definition" (definition-to-clojure tr)
      "splice" (list 'splice
                     (subexpression-to-clojure tr "expression"))
      "this" 'this
      "tuple" (vec (for [i (range (- (count tr) 1))]
                     (subexpression-to-clojure tr i)))
      "unquote" (list 'unquote
                      (subexpression-to-clojure tr "expression"))
      "unrecognized expression type")))

(defn subexpression-to-clojure [tr key]
  (let [sub (get tr key)]
    (if (= sub nil)
      (list 'missing! key)
      (to-clojure sub))))
