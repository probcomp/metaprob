;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin
  (:refer-clojure :exclude [not
                            assert
                            pprint
                            list
                            and
                            or
                            first
                            rest
                            last
                            nth
                            range
                            print])
  (:require [metaprob.environment :as env])
  (:require [metaprob.trace :refer :all])
  (:require [kixi.stats.distribution :as dist])
  (:require [kixi.stats.math :as math])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(declare length nth)

;; empty-trace? - is this trace a metaprob representation of an empty tuple/list?

(defn empty-trace? [x]
  (let [x (proper-trace x)]
    (clojure.core/and x
                      (= (trace-count x) 0)
                      (clojure.core/not (has-value? x)))))

;; --------------------
;; Let's start with metaprob tuples (= arrays), which are needed
;; for defining primitives.

(defn metaprob-tuple? [x]
  (let [x (proper-trace x)]    ; nil if not a trace with either value or subtrace
    (clojure.core/and x
                      (let [n (trace-count x)]
                        (clojure.core/or (= n 0)
                                         (clojure.core/and (has-subtrace? x 0)
                                                           (has-subtrace? x (- n 1))))))))

(defn metaprob-tuple-to-seq [tup]
  (clojure.core/assert (metaprob-tuple? tup))
  (subtrace-values-to-seq tup))

;; Convert clojure seq to metaprob tuple

(defn seq-to-metaprob-tuple [things]
  (trace-from-seq (map new-trace things)))


;; --------------------
;; Next, the minimal pair support that will be needed in order to
;; define primitives.

(declare first rest)

(def rest-marker "rest")

(defn metaprob-pair? [x]
  (let [x (proper-trace x)]
    (clojure.core/and x
                      (has-value? x)
                      (has-subtrace? x rest-marker)
                      (= (trace-count x) 1))))

(defn metaprob-cons [thing mp-list]
  (clojure.core/assert (clojure.core/or (empty-trace? mp-list)
                                        (metaprob-pair? mp-list)))
  (trace-from-map {rest-marker mp-list} thing))

;; seq-to-metaprob-list - convert clojure sequence to metaprob list.

(defn seq-to-metaprob-list [things]
  (let [things (seq things)]
    (if (empty? things)
      (new-trace)
      (metaprob-cons (clojure.core/first things)
                     (seq-to-metaprob-list (clojure.core/rest things))))))

(defn metaprob-list-to-seq [things]
  (if (metaprob-pair? things)
    (cons (value things)
          (metaprob-list-to-seq (rest things)))
    (do (clojure.core/assert (empty-trace? things))
        '())))

;; metaprob-collection-to-seq - convert metaprob collection=sequence
;; to clojure sequence.

(defn metaprob-collection-to-seq [things]
  (if (trace? things)
    (if (empty-trace? things)
      (clojure.core/list)
      (if (metaprob-pair? things)
        (metaprob-list-to-seq things)
        (metaprob-tuple-to-seq things)))
    (seq things)))

;; --------------------
;; Now ready to start defining primitives.

;; Default logpdf function

(defn default-logpdf [params answer] 0)

(declare make-simple-foreign-probprog)

;; A generator takes one argument, the metaprob parameter list.  A
;; spread-generator takes the values in the metaprob parameter list as
;; its arguments.  Think of a better name.

(defn make-foreign-probprog [name spread-generator logpdf]
  (let [name (if (symbol? name) (str name) name)]
    (clojure.core/assert (string? name))
    (clojure.core/assert (instance? clojure.lang.IFn spread-generator))
    (clojure.core/assert (instance? clojure.lang.IFn logpdf))
    (letfn [(generate [args]          ;sp.simulate
              (apply spread-generator (metaprob-collection-to-seq args)))
            (query [args intervene target output]
              (let [params (metaprob-collection-to-seq args)
                    [answer score]
                    (if (clojure.core/and target (has-value? target))
                      [(value target) (logpdf (value target) params)]
                      [(if (clojure.core/and intervene (has-value? intervene))
                         (do (clojure.core/print (format "intervene -> %s\n" (value intervene)))
                             (value intervene))
                         (apply spread-generator params))
                       0])]
                (if output (set-value! output answer))
                ;; (tuple answer score) - tuple isn't defined yet
                (trace-from-map {0 (new-trace answer) 1 (new-trace score)})))]

      (with-meta spread-generator
        {:trace (trace-from-map
                  ;; Compatibility kludge - will be simpler after removal of 
                  ;; deprecated stuff
                  (if (= logpdf default-logpdf)
                    {"name" (new-trace name)
                     "foreign-generate" (new-trace generate)
                     "foreign-query" (new-trace query)
                     }
                    {"name" (new-trace name)
                     "foreign-generate" (new-trace generate)
                     "foreign-query" (new-trace query)

                     ;; --Deprecated--
                     "custom_interpreter"
                       (new-trace (make-simple-foreign-probprog name
                                                                (fn [args intervene]
                                                                  (nth (query args intervene nil nil)
                                                                       0))))
                     "custom_choice_tracer"
                       (new-trace (make-simple-foreign-probprog name
                                                                (fn [args intervene output]
                                                                  (nth (query args intervene nil output)
                                                                       0))))
                     "custom_proposer"
                       (new-trace (make-simple-foreign-probprog name
                                                                (fn [args intervene target]
                                                                  (query args intervene target nil))))
                     ;; Recommended: change the interpreters so that
                     ;; they invoke the foreign-query property directly,
                     ;; using
                     "custom_choice_tracing_proposer"
                       (new-trace (make-simple-foreign-probprog name query))
                     })
                  "prob prog")}))))

(defn make-simple-foreign-probprog [name spread-generator]
  (make-foreign-probprog name spread-generator default-logpdf))

;; The aux-name is logically unnecessary but helps with debugging.
;; mp-name is the name the primitive has in the metaprob namespace.
;; lib-name is deprecated.

(defmacro ^:private define-nondeterministic-primitive [mp-name
                                                       generator-fun
                                                       logpdf-fun]
  (let [generator-name (symbol (str mp-name '|generate))
        logpdf-name (symbol (str mp-name '|logpdf))]
    `(do (declare ~mp-name)
         ;; It would be better if these were defns instead of defs, but
         ;; that would require work.
         ;; generator-fun takes the distribution parameters as arguments
         (def ~generator-name ~generator-fun)
         ;; logpdf-fun takes 2 args, a sample and the list of parameters
         (def ~logpdf-name ~logpdf-fun)
         (def ~mp-name
           (make-foreign-probprog '~mp-name
                                  ~generator-name
                                  ~logpdf-name)))))

(defmacro ^:private define-deterministic-primitive [mp-name params & body]
  `(define-nondeterministic-primitive ~mp-name (fn ~params ~@body) default-logpdf))

;; Metaprob interface to 'foreign' probprogs.

;; Invoke a simple 'foreign' probprog.  Two cases:
;; 1. generate - like 'simulate' in the python version.
;;    fun is the value of a probprog's "foreign-generate" property and is
;;    supposed to be a clojure function taking one argument, a metaprob
;;    collection of arguments.
;; 2. query - similar to `py_propose` in the python code.  fun is the value
;;    of a probprog's "foreign-query" property, and takes four arguments
;;    (metprob arg list + 3 traces), and returns 2 values (answer + score)
;;    as a metprob tuple.

(define-deterministic-primitive call-foreign [fun & args]
  (clojure.core/assert (instance? clojure.lang.IFn fun))
  (apply fun args))

;; Deprecated...

(define-deterministic-primitive interpret_prim [generate inputs intervention-trace]
  (clojure.core/assert (instance? clojure.lang.IFn generate))
  (if (has-value? intervention-trace)
    (value intervention-trace)
    (generate inputs)))


;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

(define-deterministic-primitive assert [condition complaint]
  (clojure.core/assert condition complaint))

(def not (make-simple-foreign-probprog 'not clojure.core/not))
(def eq (make-simple-foreign-probprog 'eq =))

(define-deterministic-primitive neq [x y] (clojure.core/not (= x y)))

(def gt (make-simple-foreign-probprog 'gt >))
(def gte (make-simple-foreign-probprog 'gte >=))
(def lte (make-simple-foreign-probprog 'lte <=))
(def lt (make-simple-foreign-probprog 'lt <))

(declare append purify)

;; Purity is contagious.

(define-deterministic-primitive add [x y]
  (if (number? x)
    (+ x y)
    (if (clojure.core/and (trace? x) (trace? y))
      (append x y)
      (if (clojure.core/and (string? x) (string? y))
        (concat x y)
        (let [to-seq (fn [x]
                       (if (string? x)
                         (clojure.core/list x) ;????
                         (let [p (purify x)]
                           (clojure.core/assert (seqable? p) ["invalid argument for add" x y])
                           p)))]
          (concat (to-seq x) (to-seq y)))))))

(def sub (make-simple-foreign-probprog 'sub -))
(def mul (make-simple-foreign-probprog 'mul *))
(def div (make-simple-foreign-probprog 'div /))

;; (def cos (make-simple-foreign-probprog 'cos clojure.core/cos))
;; (def sin (make-simple-foreign-probprog 'sin clojure.core/sin))

(define-deterministic-primitive log [x] (math/log x))
(define-deterministic-primitive cos [x] (math/cos x))
(define-deterministic-primitive sin [x] (math/sin x))

(define-deterministic-primitive empty-trace []
  (new-trace))

;; Convert a value to be used as a key to a pure clojure value so that
;; hash and = will work on it.

(defn purify [x]
  (if (trace? x)
    (if (empty-trace? x)
      '()
      (if (metaprob-pair? x)
        (apply clojure.core/list (map purify (metaprob-list-to-seq x))) ;cf. metaprob-value?
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
    addr))

;; If an object (e.g. a function) has a :trace meta-property, then
;; return that meta-property value.  Otherwise, just return the
;; object.

(defn tracify [x]
  (if (trace? x)
    x
    (let [m (meta x)]
      (if (clojure.core/and (map? m) (contains? m :trace))
        (let [tr (get m :trace)]
          (clojure.core/assert (trace? tr))
          tr)
        (clojure.core/assert false
                             ["can't coerce this to a trace" x])))))

;; OK as an argument to tracify?

(defn tracish? [x]
  (clojure.core/or (trace? x)
                   (let [m (meta x)]
                     (clojure.core/and (map? m) (contains? m :trace)))))

;; VKM name
(define-deterministic-primitive trace-get [tr]
  (cond (list? tr) (first tr)
        true (value (tracify tr))))        ; *e

(def trace_get trace-get)   ; backward compatibility name

(define-deterministic-primitive trace_has [tr]
  (cond (list? tr) (not (empty? tr))
        (vector? tr) false
        true (has-value? (tracify tr))))

;; VKM name
(define-deterministic-primitive trace-set [tr val]            ; e[e] := e
  (set-value! (tracify tr) val))

(define-deterministic-primitive trace_set [tr val] ; backward compatibility name
  (set-value! (tracify tr) val))

;; VKM name
(define-deterministic-primitive trace-subtrace [tr addr]
  (let [addr (addrify addr)]
    (if (empty? addr)
      tr
      (let [[key more-addr] addr]
        ;; Make clojure lists and vectors look like traces
        (cond (list? tr)
              (do (assert (= key "rest"))
                  (trace-subtrace (rest tr) more-addr))

              (vector? tr)
              (trace-subtrace (clojure.core/nth tr key) more-addr)

              true
              (subtrace-location-at (tracify tr) addr))))))   ; e[e]

(define-deterministic-primitive lookup [tr addr] ; backward compatibility name
  (trace-subtrace tr addr))

;; VKM proposed
(define-deterministic-primitive trace-delete [tr addr]   ; del e[e]
  (clear-value! (trace-subtrace tr addr)))

(define-deterministic-primitive trace-clear [tr] ; backward compatibility name
  (clear-value! (tracify tr)))

(def trace_clear trace-clear) ; backward compatibility name

(define-deterministic-primitive trace_set_at [tr addr val]
  (set-value-at! (tracify tr) (addrify addr) val))

(define-deterministic-primitive trace_set_subtrace_at [tr addr sub]
  (set-subtrace-at! (tracify tr) (addrify addr) sub)
  "do not use this value")

(define-deterministic-primitive trace_has_key [tr key]
  (let [key (purify key)]
    (cond (list? tr) (clojure.core/and (not (empty? tr)) (= key 'first))
          (vector? tr) (clojure.core/and (integer? key)
                            (>= key 0)
                            (< key (length tr)))
          true (has-subtrace? (tracify tr) key))))

(define-deterministic-primitive trace_subkeys [tr]
  (seq-to-metaprob-list (trace-keys (tracify tr))))

;; Translation of .sites method from trace.py.
;; Also known as addresses_of.
;; Returns a list of addresses, I believe.  (and addresses are themselves lists.)

(define-deterministic-primitive trace-get-addresses [trace]
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

(define-deterministic-primitive trace_sites [trace] ; backward compatility
  (trace-get-addresses trace))

(define-deterministic-primitive prob_prog_name [pp]
  (let [tr (tracify pp)]
    (if (has-subtrace? tr "name")
      (value (subtrace tr "name"))
      nil)))

;; Prettyprint

(defn pprint-atom [a]
  (if (tracish? a)
    (let [x (tracify a)
          keys (trace-keys x)]
      (if (has-value? x)
        (if (empty? keys)
          (clojure.core/print (format "{{%s}}" (value x)))
          (clojure.core/print (format "{{%s, %s: ...}}" (value x) (clojure.core/first keys))))
        (if (empty? keys)
          (clojure.core/print "{{}}")
          (clojure.core/print (format "{{%s: ...}}" (clojure.core/first keys))))))
    (if (string? a)
      (clojure.core/print a)    ;without quotes
      (pr a))))

(define-deterministic-primitive pprint [x]
  (if (tracish? x)
    (let [tr (tracify x)]
      (if (not (= tr x))
        (clojure.core/print "*"))
      (letfn [(re [tr indent tag]
                (clojure.core/print indent)
                (pprint-atom tag)
                (if (clojure.core/or (has-value? tr)
                                     (not (empty? (trace-keys tr))))
                  (clojure.core/print ": "))
                ;; If it has a value, clojure-print the value
                (if (has-value? tr)
                  (pprint-atom (value tr)))
                (newline)
                (let [indent (str indent "  ")]
                  (doseq [key (trace-keys tr)]
                    (re (subtrace tr key) indent key))))]
        (re tr "" "trace")))
    (do (clojure.core/print x) (newline)))
  (flush))

(def print (make-simple-foreign-probprog 'print prn))

;; Other builtins

(def rng (java.util.Random. 42))

;; Code translated from class BernoulliOutputPSP(DiscretePSP):
;;
;; The larger the weight, the more likely it is that the sample is
;; true rather than false.

(define-nondeterministic-primitive flip
  (fn
    ([] (<= (.nextDouble rng) 0.5))
    ([weight]
     (let [answer (<= (.nextDouble rng) weight)]
       (clojure.core/print (format "flip %s -> %s\n" weight answer))
       answer)))
  (fn [sample params]
    (let [weight (apply (fn ([] 0.5) ([weight] weight))
                        params)]
      (if sample
        (math/log weight)
        (java.lang.Math/log1p (- 0 weight))))))

(define-deterministic-primitive exp [x] (java.lang.Math/exp x))
(define-deterministic-primitive sqrt [x] (java.lang.Math/sqrt x))

(defn pi [x] (java.lang.Math/acos -1))

(define-deterministic-primitive normal [mu variance]
  (let [two*variance (* 2.0 variance)]
    (fn [x]
      (let [x-mu (- x mu)]
        (/ (exp (- 0 (/ (* x-mu x-mu) two*variance)))
           (sqrt (* pi two*variance)))))))

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

(define-nondeterministic-primitive beta
  (fn [a b]
    ;; From kixi/stats/distribution.cljc :
    ;; (let [[r1 r2] (split rng)
    ;;         u (rand-gamma alpha r1)]
    ;;   (/ u (+ u (rand-gamma beta r2))))
    ;; rand-gamma is hairy. but defined in same file.
    (dist/draw (dist/beta :alpha a :beta b)
               :seed (.nextLong rng)))
  (fn [x [a b]]
    ;; Venture does:
    ;; def logDensityNumeric(self, x, params):
    ;;   return scipy.stats.beta.logpdf(x,*params)
    ;; Wikipedia has a formula for pdf; easy to derive logpdf 
    ;; from it.
    ;; scipy has a better version:
    ;; https://github.com/scipy/scipy/blob/master/scipy/stats/_continuous_distns.py
    (- (+ (* (math/log x) (- a 1.0))
          (* (math/log (- 1.0 x)) (- b 1.0)))
       (log-Beta a b))))

;; Uniform

(define-nondeterministic-primitive uniform_continuous
  (fn [a b]
    (dist/draw (dist/uniform a b)))
  (fn [x [a b]]
    ;; return scipy.stats.uniform.logpdf(x, low, high-low)
    (- 0.0 (math/log (- b a)))))

;; Categorical

(define-nondeterministic-primitive uniform_categorical
  (fn [items]
    ;; items is a metaprob list (or tuple??)
    (let [n (dist/draw (dist/uniform 0 (length items)))]
      (nth items (Math/floor n))))
  (fn [item [items]]
    (let [items (metaprob-collection-to-seq items)]
      (- (math/log (count (filter (fn [x] (= x item)) items)))
         (math/log (count items))))))

;; 

(define-nondeterministic-primitive log-categorical
  (fn [scores]
    (let [weights (map exp (metaprob-collection-to-seq scores))
          normalizer (reduce + 0 weights)
          probabilities (map (fn [w] (/ w normalizer)) weights)
          sample (uniform_continuous 0 1)]
      ;; iterate over probabilities, accumulate running sum, stop when cum prob > sample.
      (letfn [(scan [i probs running]
                (let [running (+ (first probs) running)]
                  (if (> running sample)
                    i
                    (scan (+ i 1) (rest probs) running))))]
        (scan 0 probabilities 0.0))))
  (fn [i [scores]]
    (let [weights (map exp (metaprob-collection-to-seq scores))
          normalizer (reduce + 0 weights)
          probabilities (map (fn [w] (/ w normalizer)) weights)]
      (log (clojure.core/nth probabilities i)))))

;; pair - not defined in prelude

(define-deterministic-primitive pair [thing mp-list]
  (metaprob-cons thing mp-list))

;; list_to_array - convert metaprob list to metaprob array/tuple

(define-deterministic-primitive list_to_array [mp-list]
  (letfn [(r [mp-list n]
            (if (empty-trace? mp-list)
              {}
              (assoc (r (rest mp-list) (+ n 1))
                     n
                     (new-trace (first mp-list)))))]
    (trace-from-map (r mp-list 0))))

;; list - builtin

(define-deterministic-primitive list [& things]
  (seq-to-metaprob-list things))

;; addr - like list

(define-deterministic-primitive addr [& things]
  (seq-to-metaprob-list things))

;; array_to_list - builtin - metaprob array/tuple to metaprob list

(define-deterministic-primitive array_to_list [tup]
  (if (trace? tup)
    (letfn [(scan [i]
              (if (has-subtrace? tup i)
                (pair (value (subtrace tup i)) (scan (+ i 1)))
                (new-trace)))]
      (scan 0))
    ;; seqable?
    (apply list tup)))

;; This is an approximation

(define-deterministic-primitive is_metaprob_array [x]
  (metaprob-tuple? x))

;; dummy, no longer needed (used in examples and/or prelude?)

(define-deterministic-primitive dereify_tag [x]
  false)

;; In metaprob, these are strict functions.

(define-deterministic-primitive and [a b]
  (clojure.core/and a b))

(define-deterministic-primitive or [a b]
  (clojure.core/or a b))

(define-deterministic-primitive exactly [& body]
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

(define-deterministic-primitive first [mp-list]
  (if (metaprob-pair? mp-list)
    (value mp-list)
    (if (seqable? mp-list)              ;e.g. an address
      (clojure.core/first mp-list)
      (clojure.core/assert false (diagnose-nonpair mp-list)))))

;; rest - overrides original prelude (performance + generalization)

(define-deterministic-primitive rest [mp-list]
  (if (metaprob-pair? mp-list)
    (subtrace mp-list rest-marker)
    (if (seqable? mp-list)              ;e.g. an address
      (clojure.core/rest mp-list)
      (clojure.core/assert false (diagnose-nonpair mp-list)))))

;; is_pair - overrides original prelude (performance + generalization)

(define-deterministic-primitive is_pair [x]
  (metaprob-pair? x))

;; length - overrides original prelude (performance + generalization)

(define-deterministic-primitive length [x]
  (clojure.core/assert (trace? x))
  (if (empty-trace? x)
    0
    (if (metaprob-pair? x)
      (letfn [(scan [x]
                (if (metaprob-pair? x)
                  (+ 1 (scan (rest x)))
                  0))]
        (scan x))
      (do (clojure.core/assert (metaprob-tuple? x))
        (trace-count x)))))

;; drop - use prelude version?

;; last - overrides original prelude (performance + generalization)

(defn ^:private mp-list-last [mp-list]
  (if (metaprob-pair? mp-list)
    (let [more (rest mp-list)]
      (if (clojure.core/not (metaprob-pair? more))
        (first mp-list)
        (mp-list-last more)))
    mp-list))

(define-deterministic-primitive last [mp-list]
  (if (metaprob-pair? mp-list)
    (mp-list-last mp-list)
    (clojure.core/assert false (diagnose-nonpair mp-list))))

;; nth - overrides original prelude (performance + generalization)

(define-deterministic-primitive nth [thing i]
  (if (trace? thing)
    (if (metaprob-pair? thing)
      (letfn [(re [l i]
                (if (metaprob-pair? l)
                  (if (= i 0)
                    (first l)
                    (re (rest l) (- i 1)))
                  (clojure.core/assert false [(diagnose-nonpair l) i (length thing)])))]
        (re thing (int i)))
      (value (subtrace thing i)))
    (clojure.core/nth thing i)))

;; prelude has: reverse, propose1, iterate, replicate, repeat

;; range - overrides original prelude (performance + generalization)

(defn _range [n k]
  (if (>= k n) (new-trace) (pair k (_range n (+ k 1)))))

(define-deterministic-primitive range [n]
  (_range n 0))

;; map - overrides original prelude - BUT DON'T DO THIS - we need the
;; metaprob form so that we can feed it through the meta-circular
;; interpreters.

;; Attempt to make type of result be the same as type of input.
;; --> We'll want to use the version from the original prelude so that
;; the traces can propagate through to calls to the function.

(define-deterministic-primitive mp-map [mp-fn mp-seq]
  ;; Do something - need to thread the trace through.
  (let [mp-seq (tracify mp-seq)]
    (if (trace? mp-seq)
      (if (empty-trace? mp-seq)
        mp-seq
        (if (metaprob-pair? mp-seq)
          (letfn [(maplist [mp-list]
                    (clojure.core/assert (trace? mp-list) mp-list)
                    (if (empty-trace? mp-list)
                      mp-list
                      (do (clojure.core/assert (metaprob-pair? mp-list) (trace-keys mp-list))
                          (pair (mp-fn (first mp-list))
                                (maplist (rest mp-list))))))]
            (maplist mp-seq))
          ;; tbd: do this with zipmap instead of recursion
          (letfn [(maptup [i]
                    (if (has-subtrace? mp-seq i)
                      (assoc (maptup (+ i 1))
                             i
                             (new-trace (mp-fn (value (subtrace mp-seq i)))))
                      {}))]
            (trace-from-map (maptup 0) "tuple"))))
      (clojure.core/map mp-fn mp-seq))))    ;??? this isn't right

;; original prelude has: imap, zipmap, for_each, filter

;; append - overrides original prelude (performance)
;; This is only for metaprob lists, not for tuples.

(define-deterministic-primitive append [x y]
  (if (metaprob-pair? x)
    (pair (first x) (append (rest x) y))
    (do (clojure.core/assert (or (empty-trace? y)
                                 (metaprob-pair? y))
                             ["expected append 2nd arg to be a mp list" y])
        y)))

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; What about sp = tracing_proposer_to_prob_prog in prelude (!!) - do
;; we need it, how to define, etc.?

;; original prelude has: proposer_of factor apply_with_address

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; error - overrides original prelude (???)

(define-deterministic-primitive error [& irritants]
  (clojure.core/assert false irritants))                     ;from prelude.vnts

;; capture_tag_address - overrides original prelude - but definition is the same.
;; Overrides definition in prelude.clj.

(define-deterministic-primitive capture_tag_address [i t o]
  (clojure.core/assert (clojure.core/and (trace? i) (trace? t) (trace? o)))
  (trace-from-map {"intervention" (new-trace i)
                   "target" (new-trace t)
                   "output" (new-trace o)}
                  "captured tag address"))

;; resolve_tag_address - original is in builtin.py

(define-deterministic-primitive resolve_tag_address [quasi_addr]
  (let [captured (first quasi_addr)
        addr (addrify (rest quasi_addr))]
    (clojure.core/assert (trace? captured))
    (clojure.core/assert (= (trace-count captured) 3))
    (let [i (value (subtrace captured "intervention"))
          t (value (subtrace captured "target"))
          o (value (subtrace captured "output"))]
      (let [i2 (subtrace-location-at i addr)
            t2 (subtrace-location-at t addr)
            o2 (subtrace-location-at o addr)]
        (clojure.core/assert (clojure.core/and (trace? i2) (trace? t2) (trace? o2)))
        (seq-to-metaprob-tuple [i2 t2 o2])))))

(defn collection-subtraces-to-seq [coll]
  (clojure.core/assert trace? coll)
  (if (has-subtrace? coll rest-marker)
    (letfn [(re [coll]
              (if (empty-trace? coll)
                '()
                (cons coll (re (rest coll)))))]
      (re coll))
    (subtraces-to-seq coll)))


;; --------------------
;; Lexical environments, needed by program macro.

;; env_lookup - overrides original prelude

(define-deterministic-primitive env_lookup [env name]
  (env/env-lookup env (str name)))

;; make_env - overrides original prelude

(define-deterministic-primitive make_env [parent]
  (env/make-sub-environment parent))

;; match_bind - overrides original prelude.
;; Liberal in what it accepts: the input can be either a list or a
;; tuple, at any level.

(defn match-bind [pattern input env]
  ;; pattern is a trace (variable or list, I think, maybe seq)
  (clojure.core/assert trace? pattern)
  (case (value pattern)
    "variable" (env/env-bind! env (value (subtrace pattern "name")) input)
    "tuple"
    ;; input is either a metaprob list or a metaprob tuple
    (let [subpatterns (subtraces-to-seq pattern)
          parts (metaprob-collection-to-seq input)]
      (clojure.core/assert
       (= (count subpatterns) (count parts))
       ["number of subpatterns differs from number of input parts"
        (count subpatterns) (count parts)])
      ;; Ugh. https://stackoverflow.com/questions/9121576/clojure-how-to-execute-a-function-on-elements-of-two-seqs-concurently
      (doseq [[p i] (map clojure.core/list subpatterns parts)]
        (match-bind p i env)))))

(define-deterministic-primitive match_bind [pattern input env]
  (dosync (match-bind pattern input env))
  "return value of match_bind")


;; Random stuff

;; Does list s contain element x?
;; Not used

(defn metaprob-list-contains? [s x]
  (if (empty-trace? s)
    false
    (if (= x (first s))
      true
      (metaprob-list-contains? (rest s) x))))

;; All the members of s1 that are *not* in s2
;; Translation of version found in builtin.py.

(define-deterministic-primitive set-difference [s1 s2]
  (seq-to-metaprob-list
   (seq (set/difference (set (map purify
                                  (metaprob-collection-to-seq s1)))
                        (set (map purify
                                  (metaprob-collection-to-seq s2)))))))

(def set_difference set-difference)

;; -----------------------------------------------------------------------------

(define-deterministic-primitive binned-histogram [& {:keys [name samples overlay-densities]}]
  (clojure.core/prn "Binned histogram")
  (let [samples (metaprob-collection-to-seq samples)
        path (clojure.string/replace name " " "_")]
    (prn name)
    (prn (purify overlay-densities))
    (with-open [writor (io/writer (str "results/" path ".samples"))]
      (doseq [sample samples]
        (.write writor (str sample))
        (.write writor "\n"))
      (.close writor))))
