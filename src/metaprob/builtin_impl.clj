;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin-impl
  (:require [metaprob.trace :refer :all])
  (:require [kixi.stats.math :as math])
  (:require [kixi.stats.distribution :as dist])  ;for beta
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(declare length metaprob-nth metaprob-pprint metaprob-rest metaprob-first)

;; empty-trace? - is this trace a metaprob representation of an empty tuple/list?

(defn empty-trace? [x]
  (let [x (proper-trace x)]
    (and x
                      (= (trace-count x) 0)
                      (not (has-value? x)))))

;; --------------------
;; Let's start with metaprob tuples (= arrays), which are needed
;; for defining primitives.

(defn metaprob-tuple? [x]
  (let [x (proper-trace x)]    ; nil if not a trace with either value or subtrace
    (and x
                      (let [n (trace-count x)]
                        (or (= n 0)
                                         (and (has-subtrace? x 0)
                                                           (has-subtrace? x (- n 1))))))))

(defn metaprob-tuple-to-seq [tup]
  (if (metaprob-tuple? tup)
    (subtrace-values-to-seq tup)
    (do (metaprob-pprint tup)
        (assert false "^ Not a metaprob tuple"))))


;; Convert clojure seq to metaprob tuple

(defn seq-to-metaprob-tuple [things]
  (trace-from-seq (map new-trace things)))


;; --------------------
;; Next, the minimal pair support that will be needed in order to
;; define primitives.

(def rest-marker "rest")

(defn metaprob-pair? [x]
  (let [x (proper-trace x)]
    (and x
                      (has-value? x)
                      (has-subtrace? x rest-marker)
                      (= (trace-count x) 1))))

(defn metaprob-cons [thing mp-list]
  (assert (or (empty-trace? mp-list)
                                        (metaprob-pair? mp-list)))
  (trace-from-map {rest-marker mp-list} thing))

;; seq-to-metaprob-list - convert clojure sequence to metaprob list.

(defn seq-to-metaprob-list [things]
  (let [things (seq things)]
    (if (empty? things)
      (new-trace)
      (metaprob-cons (first things)
                     (seq-to-metaprob-list (rest things))))))

(defn metaprob-list-to-seq [things]
  (if (metaprob-pair? things)
    (cons (value things)
          (metaprob-list-to-seq (metaprob-rest things)))
    (do (assert (empty-trace? things))
        '())))

;; metaprob-collection-to-seq - convert metaprob collection=sequence
;; to clojure sequence.

(defn metaprob-collection-to-seq [things]
  (if (trace? things)
    (if (empty-trace? things)
      (list)
      (if (metaprob-pair? things)
        (metaprob-list-to-seq things)
        (metaprob-tuple-to-seq things)))
    (if (seqable? things)
      (seq things)
      (assert false
                           ["Metaprob-collection is neither a trace nor a seq" things]))))

;; --------------------
;; Now ready to start defining primitives.

(defn make-foreign-probprog [name ifn]
  (assert (or (string? name) (integer? name) (= name nil)) name)
  (assert (instance? clojure.lang.IFn ifn) ifn)
  (with-meta ifn
    {:trace (trace-from-map
             {"name" (new-trace name)
              "foreign-generate-method" (new-trace ifn)}
             "prob prog")}))

(defmacro ^:private define-foreign-probprog [name & rest-of-defn]
  `(defn ~name ~@rest-of-defn))

;; Invoke a foreign probprog.  Called from query-foreign in interpreter.

(define-foreign-probprog generate-foreign [ifn argseq]
  (apply ifn (metaprob-collection-to-seq argseq)))

;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

(defn metaprob-assert [condition complaint & irritants]
  (binding [*out* *err*]
    (doseq [irritant irritants]
      (if (trace? irritant)
        (do (print "Irritant:")
            (metaprob-pprint irritant)))))
  (assert condition
          (if (empty? irritants)
            complaint
            (vec (cons complaint irritants)))))

(defn neq [x y] (not (= x y)))

(def gt (make-foreign-probprog "gt" >))
(def gte (make-foreign-probprog "gte" >=))
(def lte (make-foreign-probprog "lte" <=))
(def lt (make-foreign-probprog "lt" <))

(declare append purify)

;; Purity is contagious.

(define-foreign-probprog add [x y]
  (if (number? x)
    (+ x y)
    (if (and (trace? x) (trace? y))
      (append x y)
      (if (and (string? x) (string? y))
        (concat x y)
        (let [to-seq (fn [x]
                       (if (string? x)
                         (list x) ;????
                         (let [p (purify x)]
                           (assert (seqable? p) ["invalid argument for add" x y])
                           p)))]
          (concat (to-seq x) (to-seq y)))))))

(def sub (make-foreign-probprog "sub" -))
(def mul (make-foreign-probprog "mul" *))
(def div (make-foreign-probprog "div" /))

;; (def cos (make-foreign-probprog "cos" cos))
;; (def sin (make-foreign-probprog "sin" sin))

(define-foreign-probprog log [x] (math/log x))
(define-foreign-probprog cos [x] (math/cos x))
(define-foreign-probprog sin [x] (math/sin x))
(define-foreign-probprog log1p [x] (java.lang.Math/log1p x))
(define-foreign-probprog log-gamma [x] (math/log-gamma x))

(define-foreign-probprog empty-trace []
  (new-trace))

;; Convert a value to be used as a key to a pure clojure value so that
;; hash and = will work on it.

(defn purify [x]
  (if (trace? x)
    (if (empty-trace? x)
      '()
      (if (metaprob-pair? x)
        (apply list (map purify (metaprob-list-to-seq x))) ;cf. metaprob-value?
        (if (metaprob-tuple? x)
          (vec (map purify (metaprob-tuple-to-seq x)))
          (let [keys (trace-keys x)
                maap (into {} (for [key keys] [key (purify (subtrace x key))]))]
            (if (has-value? x)
              (assoc maap :value (purify (value x)))
              maap)))))
    x))

;; addr is a metaprob list. The trace library wants clojure sequences.
;; TBD: permit tuple etc. here?  (metaprob-collection-to-seq?)

(defn addrify [addr]
  (if (trace? addr)
    (map purify (metaprob-list-to-seq addr))
    (if (string? addr)
      (list addr)
      addr)))

;; If an object (e.g. a function) has a :trace meta-property, then
;; return that meta-property value.  Otherwise, just return the
;; object.

(defn tracify [x]
  (if (trace? x)
    x
    (let [m (meta x)]
      (if (and (map? m) (contains? m :trace))
        (let [tr (get m :trace)]
          (assert (trace? tr))
          tr)
        (assert false
                             ["can't coerce this to a trace" x])))))

;; OK as an argument to tracify?

(defn tracish? [x]
  (or (trace? x)
                   (let [m (meta x)]
                     (and (map? m) (contains? m :trace)))))

;; VKM name
(define-foreign-probprog trace-get
  ([tr] (cond (list? tr) (metaprob-first tr)     ; *e
              true (value (tracify tr))))
  ([tr addr]
   (let [tr (tracify tr)
         addr (addrify addr)]
     (if (has-subtrace-at? tr addr)
       (value (subtrace-at tr addr))
       (do (metaprob-pprint tr)
           (assert false ["no such subtrace" tr addr])
           )))))

(def trace_get trace-get)   ; backward compatibility name

(define-foreign-probprog trace-has? [tr]
  (cond (list? tr) (not (empty? tr))
        (vector? tr) false
        true (has-value? (tracify tr))))

(def trace_has trace-has?)   ; backward compatibility name

;; VKM name
(define-foreign-probprog lookup [tr addr]
  (if tr
    (let [addr (addrify addr)]
      (if (empty? addr)
        tr
        (let [[key more-addr] addr]
          ;; Make clojure lists and vectors look like traces
          (cond (list? tr)
                (do (assert (= key "rest"))
                    (lookup (metaprob-rest tr) more-addr))

                (vector? tr)
                (lookup (nth tr key) more-addr)

                true    ;a mutable trace
                (subtrace-location-at (tracify tr) addr)))))   ; e[e]
    ;; A subtrace of a nil trace is nil ...
    tr))

;; VKM name
(define-foreign-probprog trace-set
  ([tr val]            ; e[e] := e
   (set-value! (tracify tr) val))
  ([tr addr val]
   (set-value! (lookup tr addr) val)))

(def trace_set trace-set) ; backward compatibility name

;; VKM proposed
(define-foreign-probprog trace-delete [tr addr]   ; del e[e]
  (clear-value! (lookup tr addr)))

(define-foreign-probprog trace-clear [tr] ; backward compatibility name
  (clear-value! (tracify tr)))

(def trace_clear trace-clear) ; backward compatibility name

(define-foreign-probprog trace-set-at [tr addr val]
  (set-value-at! (tracify tr) (addrify addr) val))

(define-foreign-probprog trace-set-subtrace-at [tr addr sub]
  (set-subtrace-at! (tracify tr) (addrify addr) sub)
  "do not use this value")

(define-foreign-probprog trace-has-key? [tr key]
  (let [key (purify key)]
    (cond (list? tr) (and (not (empty? tr)) (= key 'first))
          (vector? tr) (and (integer? key)
                            (>= key 0)
                            (< key (length tr)))
          true (has-subtrace? (tracify tr) key))))

(def trace_has_key trace-has-key?)

(define-foreign-probprog trace-subkeys [tr]
  (seq-to-metaprob-list (trace-keys (tracify tr))))

;; Translation of .sites method from trace.py.
;; Returns a list of addresses, I believe.  (and addresses are themselves lists.)

(define-foreign-probprog addresses-of [trace]
  (letfn [(sites [tr]
            ;; returns a seq of traces
            (let [site-list
                  (mapcat (fn [key]
                            (map (fn [site]
                                   (metaprob-cons key site))
                                 (sites (subtrace tr key))))
                          (trace-keys tr))]
              (if (has-value? tr)
                (cons (new-trace) site-list)
                site-list)))]       
    (let [s (sites (normalize trace))]
      (doseq [site s]
        (assert (has-value-at? trace (addrify site)) ["missing value at" site]))
      (seq-to-metaprob-list s))))

(define-foreign-probprog probprog-name [pp]
  (let [tr (tracify pp)]
    (if (has-subtrace? tr "name")
      (value (subtrace tr "name"))
      nil)))

;; Prettyprint

(declare pprint-indented)

(defn  ^:private princ [x] (print x))

(defn pprint-atom [a]
  (if (tracish? a)
    (let [x (tracify a)
          keys (trace-keys x)]
      (if (has-value? x)
        (if (empty? keys)
          (princ (format "{{%s}}" (value x)))
          (princ (format "{{%s, %s: ...}}" (value x) (first keys))))
        (if (empty? keys)
          (princ "{{}}")
          (princ (format "{{%s: ...}}" (first keys))))))
    (pr a)))

(defn pprint-seq [x indent open close]
  (princ open)
  (let [vertical? (some tracish? x)
        indent (str indent " ")]
    (letfn [(lup [x first?]
              (if (not (empty? x))
                (do (if (not first?)
                      (if vertical?
                        (do (newline)
                            (princ indent))
                        (princ " ")))
                    (pprint-indented (metaprob-first x) indent)
                    (lup (metaprob-rest x) false))))]
      (lup x true)))
  (princ close))

(defn pprint-trace [x indent]
  (let [tr (tracify x)]
    (if (not (= tr x))
      (princ "*"))
    (letfn [(re [tr indent tag]
              (princ indent)
              (pprint-atom tag)
              (if (or (has-value? tr)
                                   (not (empty? (trace-keys tr))))
                (princ ": "))
              ;; If it has a value, clojure-print the value
              (if (has-value? tr)
                (pprint-atom (value tr)))
              (newline)
              (let [indent (str indent "  ")]
                (doseq [key (trace-keys tr)]
                  (re (subtrace tr key) indent key))))]
      (re tr indent "trace"))))

(defn pprint-indented [x indent]
  (cond (empty-trace? x)
        (princ "{{}}")
        
        (metaprob-pair? x)
        (pprint-seq (metaprob-list-to-seq x) indent "(" ")")

        (metaprob-tuple? x)
        (pprint-seq (metaprob-tuple-to-seq x) indent "[" "]")

        (tracish? x)
        (pprint-trace x indent)

        (list? x)
        (pprint-seq x indent "(" ")")

        (vector? x)
        (pprint-seq x indent "[" "]")

        true
        (pprint-atom x))
  (flush))

;!!
(define-foreign-probprog metaprob-pprint [x]
  (pprint-indented x "")
  (newline)
  (flush))

;; Maybe this should print (i.e. princ) instead of pr (i.e. prin1)?

;!!
(define-foreign-probprog metaprob-print [x]
  (princ x)
  (flush))

;; Other builtins

(define-foreign-probprog exp [x] (java.lang.Math/exp x))
(define-foreign-probprog sqrt [x] (java.lang.Math/sqrt x))

(defn pi [x] (java.lang.Math/acos -1))

(define-foreign-probprog normal [mu variance]
  (let [two*variance (* 2.0 variance)]
    (fn [x]
      (let [x-mu (- x mu)]
        (/ (exp (- 0 (/ (* x-mu x-mu) two*variance)))
           (sqrt (* pi two*variance)))))))

;; pair - not defined in prelude

(define-foreign-probprog pair [thing mp-list]
  (metaprob-cons thing mp-list))

;; list-to-array - convert metaprob list to metaprob array/tuple

(define-foreign-probprog list-to-array [mp-list]
  (letfn [(r [mp-list n]
            (if (empty-trace? mp-list)
              {}
              (assoc (r (metaprob-rest mp-list) (+ n 1))
                     n
                     (new-trace (metaprob-first mp-list)))))]
    (trace-from-map (r mp-list 0))))

;; list - builtin

;!!
(define-foreign-probprog metaprob-list [& things]
  (seq-to-metaprob-list things))

;; addr - like list

(define-foreign-probprog addr [& things]
  (seq-to-metaprob-list things))

;; array-to_list - builtin - metaprob array/tuple to metaprob list

(define-foreign-probprog array-to-list [tup]
  (if (trace? tup)
    (letfn [(scan [i]
              (if (has-subtrace? tup i)
                (pair (value (subtrace tup i)) (scan (+ i 1)))
                (new-trace)))]
      (scan 0))
    ;; seqable?
    (apply list tup)))

;; This is an approximation

(define-foreign-probprog is-metaprob-array [x]
  (metaprob-tuple? x))

;; In metaprob, these are strict functions.

;!!
(define-foreign-probprog metaprob-and [a b]
  (and a b))

;!!
(define-foreign-probprog metaprob-or [a b]
  (or a b))

(define-foreign-probprog exactly [& body]
  (assert false "what is exactly, exactly?"))

;; ----------------------------------------------------------------------

(defn diagnose-nonpair [mp-list]
  (if (trace? mp-list)
    (if (has-value? mp-list)
      (if (has-subtrace? mp-list rest-marker)
        "ok"
        ["no rest marker" (trace-keys mp-list)])
      ["no value" (trace-keys mp-list)])
    ["not a trace" mp-list]))

;; Defined in original prelude (if they are here, then there should be
;; some good reason not to use the prelude version)

;; first - overrides original prelude (performance + generalization)

;!!
(define-foreign-probprog metaprob-first [mp-list]
  (if (metaprob-pair? mp-list)
    (value mp-list)
    (if (seqable? mp-list)              ;e.g. an address
      (first mp-list)
      (assert false (diagnose-nonpair mp-list)))))

;; rest - overrides original prelude (performance + generalization)

;!!
(define-foreign-probprog metaprob-rest [mp-list]
  (if (metaprob-pair? mp-list)
    (subtrace mp-list rest-marker)
    (if (seqable? mp-list)              ;e.g. an address
      (rest mp-list)
      (assert false (diagnose-nonpair mp-list)))))

;; is-pair - overrides original prelude (performance + generalization)

(define-foreign-probprog is-pair [x]
  (metaprob-pair? x))

;; length - overrides original prelude (performance + generalization)

;!!
(define-foreign-probprog length [x]
  (assert (trace? x))
  (if (empty-trace? x)
    0
    (if (metaprob-pair? x)
      (letfn [(scan [x]
                (if (metaprob-pair? x)
                  (+ 1 (scan (metaprob-rest x)))
                  0))]
        (scan x))
      (do (assert (metaprob-tuple? x))
          (trace-count x)))))

;; drop - use prelude version?

;; last - overrides original prelude (performance + generalization)

(defn ^:private mp-list-last [mp-list]
  (if (metaprob-pair? mp-list)
    (let [more (metaprob-rest mp-list)]
      (if (not (metaprob-pair? more))
        (metaprob-first mp-list)
        (mp-list-last more)))
    mp-list))

;!!
(define-foreign-probprog metaprob-last [mp-list]
  (if (metaprob-pair? mp-list)
    (mp-list-last mp-list)
    (assert false (diagnose-nonpair mp-list))))

;; nth - overrides original prelude (performance + generalization)

(define-foreign-probprog metaprob-nth [thing i]
  (if (trace? thing)
    (if (metaprob-pair? thing)
      (letfn [(re [l i]
                (if (metaprob-pair? l)
                  (if (= i 0)
                    (metaprob-first l)
                    (re (metaprob-rest l) (- i 1)))
                  (assert false [(diagnose-nonpair l) i (length thing)])))]
        (re thing (int i)))
      (value (subtrace thing i)))
    (nth thing i)))

;; prelude has: reverse, propose1, iterate, replicate, repeat

;; range - overrides original prelude (performance + generalization)

(defn _range [n k]
  (if (>= k n) (new-trace) (pair k (_range n (+ k 1)))))

(define-foreign-probprog metaprob-range [n]
  (_range n 0))

;; map - overrides original prelude - BUT DON'T DO THIS - we need the
;; metaprob form so that we can feed it through the meta-circular
;; interpreters.

;; Attempt to make type of result be the same as type of input.
;; --> We'll want to use the version from the original prelude so that
;; the traces can propagate through to calls to the function.

(define-foreign-probprog mp-map [mp-fn mp-seq]
  ;; Do something - need to thread the trace through.
  (let [mp-seq (tracify mp-seq)]
    (if (trace? mp-seq)
      (if (empty-trace? mp-seq)
        mp-seq
        (if (metaprob-pair? mp-seq)
          (letfn [(maplist [mp-list]
                    (assert (trace? mp-list) mp-list)
                    (if (empty-trace? mp-list)
                      mp-list
                      (do (assert (metaprob-pair? mp-list) (trace-keys mp-list))
                          (pair (mp-fn (metaprob-first mp-list))
                                (maplist (metaprob-rest mp-list))))))]
            (maplist mp-seq))
          ;; tbd: do this with zipmap instead of recursion
          (letfn [(maptup [i]
                    (if (has-subtrace? mp-seq i)
                      (assoc (maptup (+ i 1))
                             i
                             (new-trace (mp-fn (value (subtrace mp-seq i)))))
                      {}))]
            (trace-from-map (maptup 0) "tuple"))))
      (map mp-fn mp-seq))))    ;??? this isn't right

;; original prelude has: imap, zipmap, for_each, filter

;; append - overrides original prelude (performance)
;; This is only for metaprob lists, not for tuples.

(define-foreign-probprog append [x y]
  (if (metaprob-pair? x)
    (pair (metaprob-first x) (append (metaprob-rest x) y))
    (do (assert (or (empty-trace? y)
                                 (metaprob-pair? y))
                             ["expected append 2nd arg to be a mp list" y])
        y)))

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; What about sp = tracing_proposer_to_prob_prog in prelude (!!) - do
;; we need it, how to define, etc.?

;; original prelude has: proposer_of factor apply_with_address

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; error - overrides original prelude (???)

(define-foreign-probprog error [& irritants]
  (assert false irritants))                     ;from prelude.vnts

;; capture_tag_address - overrides original prelude - but definition is the same.
;; Overrides definition in prelude.clj.

(define-foreign-probprog capture-tag-address [i t o]
  (assert (and (or (= i nil) (trace? i))
                                         (or (= t nil) (trace? t))
                                         (or (= o nil) (trace? o))))
  (trace-from-map {"intervention" (new-trace i)
                   "target" (new-trace t)
                   "output" (new-trace o)}
                  "captured tag address"))

;; resolve_tag_address - original is in builtin.py

(define-foreign-probprog resolve-tag-address [quasi_addr]
  (let [captured (metaprob-first quasi_addr)
        addr (addrify (metaprob-rest quasi_addr))]
    (assert (trace? captured))
    (assert (= (trace-count captured) 3))
    (let [i (value (subtrace captured "intervention"))
          t (value (subtrace captured "target"))
          o (value (subtrace captured "output"))]
      (let [i2 (if i (subtrace-location-at i addr) nil)
            t2 (if t (subtrace-location-at t addr) nil)
            o2 (if o (subtrace-location-at o addr) nil)]
        (assert (and (or (trace? i2) (= i2 nil))
                                               (or (trace? t2) (= t2 nil))
                                               (or (trace? o2) (= o2 nil))))
        (seq-to-metaprob-tuple [i2 t2 o2])))))

(defn collection-subtraces-to-seq [coll]
  (assert trace? coll)
  (if (has-subtrace? coll rest-marker)
    (letfn [(re [coll]
              (if (empty-trace? coll)
                '()
                (cons coll (re (metaprob-rest coll)))))]
      (re coll))
    (subtraces-to-seq coll)))

;; --------------------
;; Top level env = namespace.

(defn make-top-level-env [ns]
  (if (symbol? ns)
    (find-ns ns)
    ns))

(defn top-level-lookup [the-ns name]
  (let [v (ns-resolve the-ns (symbol name))]
    (assert (var? v) ["not bound" name the-ns])
    (assert (not (get (meta v) :macro)) ["reference to macro" name the-ns])
    (deref v)))

(defn top-level-bind! [the-ns name value]
  ;; how to create a new binding in a namespace (a la def)???
  (let [sym (symbol name)
        r (ns-resolve the-ns sym)
        r (if r r (binding [*ns* the-ns]
                    (print (format "Assigning %s in %s" sym the-ns))
                    (eval `(def ~sym))
                    (ns-resolve the-ns sym)))]
    (ref-set r value)
    nil))

;; Lexical environments, needed by program macro.

(defn frame? [obj]
  (and (trace? obj)
       (has-value? obj)
       (= (value obj) "frame")))

(defn frame-parent [frame]
  (value (subtrace frame "parent")))

;; env_lookup - overrides original prelude

(define-foreign-probprog env-lookup [env name]
  (if (frame? env)
    (if (has-subtrace? env name)
      (value (subtrace env name))
      (env-lookup (frame-parent env) name))
    ;; Top level environment
    (top-level-lookup env name)))

;; make_env - overrides original prelude

(define-foreign-probprog make-env [parent]
  (trace-from-map {"parent" (new-trace parent)}
                  "frame"))

(defn env-bind! [env name val]
  (if false
    (top-level-bind! env name val)
    (trace-set env name val)))

;; match_bind - overrides original prelude.
;; Liberal in what it accepts: the input can be either a list or a
;; tuple, at any level.

(defn match-bind [pattern input env]
  (letfn [(re [pattern input]
            ;; pattern is a trace (variable or list, I think, maybe seq)
            (assert trace? pattern)
            (case (value pattern)
              "variable" (env-bind! env (value (subtrace pattern "name")) input)
              "tuple"
              ;; input is either a metaprob list or a metaprob tuple
              (let [subpatterns (subtraces-to-seq pattern)
                    parts (metaprob-collection-to-seq input)]
                (assert
                 (= (count subpatterns) (count parts))
                 ["number of subpatterns differs from number of input parts"
                  (count subpatterns) (count parts)])
                ;; Ugh. https://stackoverflow.com/questions/9121576/clojure-how-to-execute-a-function-on-elements-of-two-seqs-concurently
                (doseq [[p i] (map list subpatterns parts)]
                  (re p i)))))]
    (dosync (re pattern input))
    "return value of match_bind"))

(def match_bind match-bind)


;; Random stuff

;; Does list s contain element x?
;; Not used

(defn metaprob-list-contains? [s x]
  (if (empty-trace? s)
    false
    (if (= x (metaprob-first s))
      true
      (metaprob-list-contains? (metaprob-rest s) x))))

;; All the members of s1 that are *not* in s2
;; Translation of version found in builtin.py.

(define-foreign-probprog set-difference [s1 s2]
  (seq-to-metaprob-list
   (seq (set/difference (set (map purify
                                  (metaprob-collection-to-seq s1)))
                        (set (map purify
                                  (metaprob-collection-to-seq s2)))))))

(def set_difference set-difference)

;; -----------------------------------------------------------------------------

(define-foreign-probprog binned-histogram [& {:keys [name samples overlay-densities]}]
  (let [samples (metaprob-collection-to-seq samples)
        path (clojure.string/replace name " " "_")]
    (print (format "Writing samples to %s for histogram generation\n" path))
    (print (format " overlay-densities = %s\n" (purify overlay-densities)))
    (with-open [writor (io/writer (str "results/" path ".samples"))]
      (doseq [sample samples]
        (.write writor (str sample))
        (.write writor "\n"))
      (.close writor))))

;; -----------------------------------------------------------------------------

;; Builtins that return scores.
;; Ideally this entire section is written in metaprob, not clojure.

(declare boot-generate mini-query mini-query-lifted)

;; 'query' specialized for foreign probprogs.  Compare query.clj.
;; Must always run the function, both in case it has side effects,
;; and so that it populates the output trace.

(define-foreign-probprog query-foreign [ifn argseq intervene target output]
  (let [answer (apply ifn (metaprob-collection-to-seq argseq))
        answer (if (if target (trace-has? target) false)
                 (trace-get target)
                 (if (if intervene (trace-has? intervene) false)
                   (trace-get intervene)
                   answer))]
    (if output
      (trace-set output answer))
    [answer 0]))

;; Temporary substitute for query-lifted (see query.clj).

(defn mini-query-lifted [query-method inputs intervene target output]
  (let [result+score+score (mini-query query-method
                                       [inputs intervene target output]
                                       nil nil nil)]
    (metaprob-nth result+score+score 0)))

;; Invoke a probprog, excluding the native case, which won't work yet,
;; because the interpreter would be a forward reference and clojure
;; hates forward references.

(defn mini-query [prog argseq i t o]
  (let [prog (tracify prog)]
    (if (has-subtrace? prog "query-method")    ;Lifted
      (mini-query-lifted (trace-get prog "query-method") argseq i t o)
      (if (has-subtrace? prog "foreign-generate-method")
        ;; Ignore the traces
        (do (print (format "Discarding traces for %s (mini-query)\n"
                           (trace-get prog "name")))
            (query-foreign (value (subtrace prog "foreign-generate-method")) argseq i t o))
        (assert false ["don't know how to query this kind of probprog" prog])))))

(defn boot-generate [pp & args]
  (apply pp args))   ; ?? metaprob-collection-to-seq ?? mini-query

;; boot-query starts out doing mini-query, then expands to query when query is ready.

(def registered-query-implementation-ref (ref mini-query))
(defn register-query-implementation! [impl]
  (dosync (ref-set registered-query-implementation-ref impl)))

;; Call the currently registered implementation for `query`

(defn boot-query [pp argseq i t o]
  ((deref registered-query-implementation-ref)
   pp
   (if (tracish? argseq)
     (tracify argseq)
     (seq-to-metaprob-list argseq))
   i t o))

;; Custom applicators

;; Lift a probprog (`query-method`) up to be a meta-probprog.
;; Write this in metaprob !!??

(define-foreign-probprog make-lifted-probprog [name query-method]
  (with-meta (fn [& argseq]
               ;; (print (format "? Calling %s (lifted) from clojure\n" name))
               (let [answer+score
                     (boot-generate query-method argseq nil nil nil)]
                 (metaprob-nth answer+score 0)))
    {:trace (trace-from-map
             {"name" (new-trace name)
              "query-method" (new-trace query-method)}
             "prob prog")}))

;; Make a bare trace be clojure-callable.
;; This ought to clojure-compile the probprog, if it has source ? ???

(defn trace-to-probprog [tr]
  (if (trace? tr)
    (with-meta (fn [& argseq]
                 ;; Foo.  We want to avoid interpreting this thing, if possible.
                 (print (format "? Calling %s (trace) from clojure\n"
                                             (trace-get tr "name")))
                 (let [[answer _]
                       (metaprob-collection-to-seq
                        (boot-query
                         tr
                         (seq-to-metaprob-list argseq)
                         nil nil nil))]
                   answer))
      {:trace tr})
    tr))

;; This is a kludge, to use until there's a better solution

(define-foreign-probprog export-probprog [prog]
  (let [name (trace-get prog "name")
        ifn (with-meta prog nil)]
    (with-meta ifn {:name name
                    :trace (trace-from-map {"name" (new-trace name)
                                            "foreign-generate-method" (new-trace ifn)}
                                           "prob prog")})))

;; -----------------------------------------------------------------------------
;; Distributions (nondeterministic probprogs)

;; Provide a probprog with a score method.
;; The score-method can be an IFn here, but it can also be a
;; probprog with the appropriate meta magic to make it callable.

;; Arguments to score-method are [sample params]

;(define provide-score-method
;  (probprog [name prog score-method]
;      (make-lifted-probprog
;         name
;         ;; This is the lifted-probprog's query-method:
;         (export-probprog
;          (probprog [argseq i t o]
;            (print o)
;            (define [answer score]
;              (query prog argseq i t o))
;            (tuple answer (score-method answer argseq)))))))


(define-foreign-probprog provide-score-method [name prog score-method]
  (let [namestring (if (symbol? name) (str name) name)]
    (make-lifted-probprog
       namestring
       ;; This is the lifted-probprog's query-method:
       (make-foreign-probprog (str namestring "|query-method")
                              (fn [argseq i t o]
                                (let [answer+score
                                      (boot-query prog argseq i t o)
                                      answer (metaprob-nth answer+score 0)]
                                  [answer
                                   (boot-generate score-method answer argseq)]))))))

;; Convenience macro.  Generate-method and score-method are both IFns.

(defmacro ^:private define-foreign-probprog-with-score [mp-name
                                                        generate-method
                                                        score-method]
  (let [namestring (if (symbol? mp-name) (str mp-name) mp-name)]
    `(def ~mp-name
       (provide-score-method ~namestring
                             (make-foreign-probprog ~(str namestring "|generate")
                                                    ~generate-method)
                             (make-foreign-probprog ~(str namestring "|score")
                                                    ~score-method)))))

(def ^:dynamic *rng* (java.util.Random. 42))

;; Uniform
;; Taylor points out that the Java RNG is not as random as one might want
;; when sampling very close to zero.

(defn sample-uniform
  ([] (.nextDouble *rng*))
  ([a b] (+ (* (.nextDouble *rng*) (- b a)) a)))

(define-foreign-probprog-with-score uniform
  sample-uniform
  (fn [x params]
    (let [[a b] (metaprob-collection-to-seq params)]
      ;; return scipy.stats.uniform.logpdf(x, low, high-low)
      (- 0.0 (math/log (- b a))))))

;; Code translated from class BernoulliOutputPSP(DiscretePSP):
;;
;; The larger the weight, the more likely it is that the sample is
;; true rather than false.

(define-foreign-probprog-with-score flip
  (fn flip
    ([] (<= (sample-uniform 0 1) 0.5))
    ([weight]
     (let [answer (<= (sample-uniform 0 1) weight)]
       (print (format "flip %s -> %s\n" weight answer))
       answer)))
  (fn [sample params]
    (let [params (metaprob-collection-to-seq params)
          weight (apply (fn ([] 0.5)
                          ([weight] weight))
                        params)]
      (if sample
        (math/log weight)
        (java.lang.Math/log1p (- 0 weight))))))

;; Categorical

(define-foreign-probprog-with-score uniform-sample
  (fn uniform-sample [items]
    ;; items is a metaprob list (or tuple??)
    (let [n (sample-uniform 0 (length items))]
      (metaprob-nth items (Math/floor n))))
  (fn [item params]
    (let [[items] (metaprob-collection-to-seq params)
          items (metaprob-collection-to-seq items)]
      (- (math/log (count (filter (fn [x] (= x item)) items)))
         (math/log (count items))))))

;; 

(define-foreign-probprog-with-score log-categorical
  (fn log-categorical [scores]
    (let [weights (map exp (metaprob-collection-to-seq scores))
          normalizer (reduce + 0 weights)
          probabilities (map (fn [w] (/ w normalizer)) weights)
          sample (uniform 0 1)]
      ;; iterate over probabilities, accumulate running sum, stop when cum prob > sample.
      (letfn [(scan [i probs running]
                (let [running (+ (metaprob-first probs) running)]
                  (if (> running sample)
                    i
                    (scan (+ i 1) (metaprob-rest probs) running))))]
        (scan 0 probabilities 0.0))))
  (fn [i params]
    (let [[scores] (metaprob-collection-to-seq params)
          weights (map exp (metaprob-collection-to-seq scores))
          normalizer (reduce + 0 weights)
          probabilities (map (fn [w] (/ w normalizer)) weights)]
      (log (nth probabilities i)))))

;; We probably don't need or even want beta.

;; Big Beta, an auxiliary used in the calculation of the PDF of a
;; beta distribution, can be calculated using Gamma.  Its log can
;; be calculated using Gamma's log, which kixi provides us.
;;
;; scipy's version is much more robust, and can be found here:
;; https://github.com/scipy/scipy/blob/master/scipy/special/cdflib/betaln.f

(defn log-Beta [a b]
  (- (+ (math/log-gamma a) (math/log-gamma b))
     (math/log-gamma (+ a b))))

;; BetaOutputPSP(RandomPSP)
;; Please read the comments in lite/continuous.py in Venture

(define-foreign-probprog-with-score beta
  (fn beta [a b]
    ;; From kixi/stats/distribution.cljc :
    ;; (let [[r1 r2] (split *rng*)
    ;;         u (rand-gamma alpha r1)]
    ;;   (/ u (+ u (rand-gamma beta r2))))
    ;; rand-gamma is hairy. but defined in same file.
    (dist/draw (dist/beta :alpha a :beta b)
               {:seed (.nextLong *rng*)}))
  (fn [x params]
    (let [[a b] (metaprob-collection-to-seq params)]
      ;; Venture does:
      ;; def logDensityNumeric(self, x, params):
      ;;   return scipy.stats.beta.logpdf(x,*params)
      ;; Wikipedia has a formula for pdf; easy to derive logpdf 
      ;; from it.
      ;; scipy has a better version:
      ;; https://github.com/scipy/scipy/blob/master/scipy/stats/_continuous_distns.py
      (- (+ (* (math/log x) (- a 1.0))
            (* (math/log (- 1.0 x)) (- b 1.0)))
         (log-Beta a b)))))

