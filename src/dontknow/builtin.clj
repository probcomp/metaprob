;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns dontknow.builtin
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
                            range])
  (:require [dontknow.trie :refer :all])
  (:require [clojure.test.check.random :as random])
  (:require [kixi.stats.distribution :as dist])
  (:require [kixi.stats.math :as math]))

;; --------------------
;; Let's start with metaprob tuples (= arrays), which are needed
;; for defining primitives.

;; empty-trace? - is this trace a metaprob representation of an empty tuple?

(defn empty-trace? [x]
  (clojure.core/and (trace? x)
                    (= (trie-count x) 0)
                    (clojure.core/not (has-value? x))))

(defn metaprob-tuple? [x]
  (clojure.core/and (trace? x)
                    (let [n (trie-count x)]
                      (clojure.core/or (= n 0)
                                       (clojure.core/and (has-subtrie? x 0)
                                                         (has-subtrie? x (- n 1)))))))

(defn metaprob-tuple-to-seq [tup]
  (clojure.core/assert (metaprob-tuple? tup))
  (subtrie-values-to-seq tup))

;; [n.v. seq-to-metaprob-tuple seems to not be needed yet; included
;; here for completeness]

(defn seq-to-metaprob-tuple [things]
  (trie-from-seq (map new-trie things)))


;; --------------------
;; Next, the minimal pair support that will be needed in order to
;; define primitives.

(def rest-marker "rest")

(defn metaprob-pair? [x]
  (clojure.core/and (trace? x)
                    (has-value? x)
                    (has-subtrie? x rest-marker)))

(defn metaprob-cons [thing mp-list]
  (clojure.core/assert (clojure.core/or (empty-trace? mp-list)
                                        (metaprob-pair? mp-list)))
  (trie-from-map {rest-marker mp-list} thing))

(defn mk_nil [] (new-trie))                 ; {{ }}

;; seq-to-metaprob-list - convert clojure sequence to metaprob list.
;; (n.b. seq-to-metaprob-tuple is defined in trie.clj)

(defn seq-to-metaprob-list [things]
  (if (empty? things)
    (mk_nil)
    (metaprob-cons (clojure.core/first things)
                   (seq-to-metaprob-list (clojure.core/rest things)))))

(defn metaprob-list-to-seq [things]
  (if (metaprob-pair? things)
    (cons (value things)
          (metaprob-list-to-seq (subtrie things rest-marker)))
    '()))

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

;; Similar to sp_to_prob_prog

(defn make-deterministic-primitive [name fun]
  (letfn [(execute [args]          ;sp.simulate
            (apply fun (metaprob-collection-to-seq args)))]
    (with-meta fun
      {:trace (trie-from-map {"name" (new-trie name)
                              "executable" (new-trie execute)}
                             "prob prog")})))

;; The aux-name is logically unnecessary but helps with debugging.
;; mp-name is the name the primitive has in the metaprob namespace.
;; lib-name is deprecated.

(defmacro ^:private define-deterministic-primitive [mp-name params & body]
  (let [aux-name (symbol (str mp-name '|primitive))]
    `(do (declare ~mp-name)
         (defn ~aux-name ~params ~@body)
         (def ~mp-name
           (make-deterministic-primitive '~mp-name
                                         ~aux-name)))))

;; This corresponds to the py_propose method of the SPFromLite class,
;; which is defined in sp.py.  'params' (elsewhere called 'args') are
;; the parameters to the particular distribution we're sampling.

(defn apply-nondeterministic [fun params log-density intervene target output]
  (let [params (metaprob-collection-to-seq params)]
    (let [[ans score]
          (if (has-value? target)
            (let [ans (value target)]
              [ans (log-density ans params)])
            (let [ans (if (has-value? intervene)
                        (value intervene)
                        (apply fun params))]
              [ans 0]))]
      (set-value! output ans)
      (metaprob-cons ans (metaprob-cons score (mk_nil))))))

;; Nondeterministic primitives have to have a suite of custom
;; application methods, one for each meta-circular interpreter.
;; def sp_to_prob_prog(name, sp):
;;   return make_prob_prog(
;;     name=name,
;;     simulator=sp.simulate,
;;     interpreter=python_interpreter_prob_prog(sp.py_propose),
;;     tracer=python_tracer_prob_prog(sp.py_propose),
;;     proposer=python_proposer_prob_prog(sp.py_propose),
;;     ptc=python_ptc_prob_prog(sp.py_propose)) # TODO Name the proposer?
;; p-a-t-c, in particular, looks for "custom_choice_tracing_proposer".

(defn make-nondeterministic-primitive [name sampler logdensity]
  (clojure.core/assert (instance? clojure.lang.IFn sampler))
  (clojure.core/assert (instance? clojure.lang.IFn logdensity))
  (letfn [(execute [args]          ;sp.simulate
            (apply sampler args))
          (p-a-t-c [args intervene target output]
            (apply-nondeterministic sampler args logdensity intervene target output))
          (interpret [args intervene]
            (p-a-t-c args intervene (mk_nil) (mk_nil)))
          (propose [args intervene target]
            (p-a-t-c args intervene target (mk_nil)))
          (trace [args intervene output]
            (p-a-t-c args intervene (mk_nil) output))]
    (with-meta sampler
      {:trace (trie-from-map
               {"name" (new-trie name)
                "execute" (new-trie (make-deterministic-primitive name execute))
                "custom_interpreter" (new-trie (make-deterministic-primitive name interpret))
                "custom_choice_trace" (new-trie (make-deterministic-primitive name trace))
                "custom_proposer" (new-trie (make-deterministic-primitive name propose))
                "custom_choice_tracing_proposer"
                    (new-trie (make-deterministic-primitive name p-a-t-c))}
               "prob prog")})))

(defmacro ^:private define-nondeterministic-primitive [mp-name
                                                       sample-fun
                                                       logdensity-fun]
  (let [sample-name (symbol (str mp-name '|sampler))
        logdensity-name (symbol (str mp-name '|logdensity))]
    `(do (declare ~mp-name)
         ;; It would be better if these were defns instead of defs, but
         ;; that would require work.
         ;; sample-fun takes the distribution parameters as arguments
         (def ~sample-name ~sample-fun)
         ;; logdensity-fun takes 2 args, a sample and the list of parameters
         (def ~logdensity-name ~logdensity-fun)
         (def ~mp-name
           (make-nondeterministic-primitive '~mp-name
                                            ~sample-name
                                            ~logdensity-name)))))



;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

(define-deterministic-primitive assert [condition complaint]
  (clojure.core/assert condition complaint))

(def not (make-deterministic-primitive 'not clojure.core/not))
(def eq (make-deterministic-primitive 'eq =))

(define-deterministic-primitive neq [x y] (clojure.core/not (= x y)))

(def gt (make-deterministic-primitive 'gt >))
(def gte (make-deterministic-primitive 'gte >=))
(def lte (make-deterministic-primitive 'lte <=))
(def lt (make-deterministic-primitive 'lt <))

(declare append)

(define-deterministic-primitive add [x y]
  (if (number? x)
    (+ x y)
    (if (trace? x)
      (append x y)
      (if (seqable? x)
        (concat x y)
        (clojure.core/assert false ["invalid argument for add" x])))))

(def sub (make-deterministic-primitive 'sub -))
(def mul (make-deterministic-primitive 'mul *))
(def div (make-deterministic-primitive 'div /))

(def mk_nil (make-deterministic-primitive 'mk_nil mk_nil))

(declare first rest)

;; addr is a metaprob list. The trie library wants clojure lists.
;; TBD: permit tuple etc. here?  (metaprob-collection-to-seq?)

(defn addrify [addr]
  (if (trace? addr)
    (if (metaprob-pair? addr)
      ;; N.b. first and rest refer to metaprob first and rest
      (cons (first addr)
            (addrify (rest addr)))
      '())
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

(define-deterministic-primitive trace_get [tr] (value (tracify tr)))        ; *e
(define-deterministic-primitive trace_has [tr] (has-value? (tracify tr)))
(define-deterministic-primitive trace_set [tr val]            ; e[e] := e
  (set-value! (tracify tr) val))
(define-deterministic-primitive trace_set_at [tr addr val]
  (set-value-at! (tracify tr) (addrify addr) val))
(define-deterministic-primitive trace_set_subtrace_at [tr addr sub]
  (set-subtrie-at! (tracify tr) (addrify addr) sub))
(define-deterministic-primitive trace_has_key [tr key]
  (has-subtrie? (tracify tr) key))
(define-deterministic-primitive trace_subkeys [tr] (trie-keys (tracify tr)))
(define-deterministic-primitive lookup [tr addr]
  ;; addr might be a metaprobe seq instead of a clojure seq.
  (subtrace-at (tracify tr) (addrify addr)))  ; e[e]

(define-deterministic-primitive prob_prog_name [pp]
  (let [tr (tracify pp)]
    (if (has-subtrie? tr "name")
      (value (subtrie tr "name"))
      nil)))

;; Deterministic apply, like 'simulate' in the python version.
;; exec is supposed to take one argument, a metaprob collection of arguments.

(define-deterministic-primitive interpret_prim [exec inputs intervention-trace]
  (clojure.core/assert (instance? clojure.lang.IFn exec))
  (if (has-value? intervention-trace)
    (value intervention-trace)
    (exec (metaprob-collection-to-seq inputs))))

;; Experimental code... probably not too hard to regenerate; flush.
;;       (let [tr (tracify exec)]
;;         (assert trace? tr)
;;         (if (has-subtrie? tr "executable")
;;           (let [f (subtrie tr "executable")]
;;             (assert (instance? clojure.lang.IFn f)
;;                     "executable property should be a clojure function")
;;             (f args))
;;           (if (clojure.core/and (has-subtrie? tr "pattern")
;;                                 (has-subtrie? tr "source"))
;;             ;; Using program-to-clojure here would be a cyclic dependency!
;;             (assert false "clojure eval of probprog not yet implemented")
;;             ;; (let [prog (program-to-clojure (trace_get (lookup tr (list "pattern"))))]
;;             ;;   (apply prog args))
;;             ;; but need to convert list to metaprob list
;;             )))

;; Used for the "executable" case in propose_and_trace_choices.
;; From metaprob/src/builtin.py :
;; def interpret_prim(f, args, intervention_trace):
;;   if intervention_trace.has():
;;     return intervention_trace.get()
;;   else:
;;     return f(metaprob_collection_to_python_list(args))


(define-deterministic-primitive pprint [x]
  ;; x is a trie.  need to prettyprint it somehow.
  (print (format "[prettyprint %s]\n" x)))

                                        ; Other builtins

(def rng (random/make-random 42))

;; Code translated from class BernoulliOutputPSP(DiscretePSP):
;;
;; The larger the weight, the more likely it is that the sample is
;; true rather than false.

(define-nondeterministic-primitive flip
  (fn
    ([] (<= (random/rand-double rng) 0.5))
    ([weight] (<= (random/rand-double rng) weight)))
  (fn [sample params]
    (let [weight (apply (fn ([] 0.5) ([weight] weight))
                        params)]
      (if sample
        (java.lang.Math/log weight)
        (java.lang.Math/log1p (- 0 weight))))))

(defn exp [x] (java.lang.Math/exp x))
(defn sqrt [x] (java.lang.Math/sqrt x))

(defn pi [x] (java.lang.Math/acos -1))

(defn normal [mu variance]
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
               :seed (random/rand-long rng)))
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

(define-deterministic-primitive uniform-continuous [a b]
  (+ (rand (- b a)) a))

;; TBD (needed by prelude):
;; trace_sites uniform_categorical uniform_continuous

(define-deterministic-primitive resolve_tag_address [stuff]
  stuff)

(define-deterministic-primitive name_for_definiens [pattern]
  (if (symbol? pattern)
    (if (= pattern '_)
      `definiens
      pattern)
    `definiens))

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
                     (new-trie (first mp-list)))))]
    (trie-from-map (r mp-list 0))))

;; list - builtin

(define-deterministic-primitive list [& things]
  (seq-to-metaprob-list things))

;; array_to_list - builtin - metaprob array/tuple to metaprob list

(define-deterministic-primitive array_to_list [tup]
  (if (trie? tup)
    (letfn [(scan [i]
              (if (has-subtrie? tup i)
                (pair (value (subtrie tup i)) (scan (+ i 1)))
                (mk_nil)))]
      (scan 0))
    ;; seqable?
    (apply list tup)))

;; This is an approximation

(define-deterministic-primitive is_metaprob_array [x]
  (metaprob-tuple? x))

;; dummy, no longer needed

(define-deterministic-primitive dereify_tag [x]
  x)

;; In metaprob, these are strict functions.

(define-deterministic-primitive and [a b]
  (clojure.core/and a b))

(define-deterministic-primitive or [a b]
  (clojure.core/or a b))

(define-deterministic-primitive exactly [& body]
  (assert false "what is exactly, exactly?"))

;; ----------------------------------------------------------------------
;; Defined in original prelude (if they are here, then there should be
;; some good reason not to use the prelude version)

;; first - overrides original prelude (performance + generalization)

(define-deterministic-primitive first [mp-list]
  (if (trace? mp-list)
    (value mp-list)
    (clojure.core/first mp-list)))

;; rest - overrides original prelude (performance + generalization)

(define-deterministic-primitive rest [mp-list]
  (if (trace? mp-list)
    (do (clojure.core/assert (metaprob-pair? mp-list))
        (subtrie mp-list rest-marker))
    (clojure.core/rest mp-list)))

;; is_pair - overrides original prelude (performance + generalization)

(define-deterministic-primitive is_pair [x]
  (metaprob-pair? x))

;; length - overrides original prelude (performance + generalization)

(define-deterministic-primitive length [x]
  (if (trace? x)
    (if (metaprob-pair? x)
      (letfn [(scan [x]
                (if (metaprob-pair? x)
                  (+ 1 (scan (rest x)))
                  0))]
        (scan x))
      (trie-count x))
    (count x)))

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
  (if (trace? mp-list)
    (mp-list-last mp-list)
    (clojure.core/last mp-list)))

;; nth - overrides original prelude (performance + generalization)

(defn mp-list-nth [mp-list i]
  (if (= i 0)
    (first mp-list)
    (mp-list-nth (rest mp-list) (- i 1))))

(define-deterministic-primitive nth [thing i]
  (if (trace? thing)
    (if (metaprob-pair? thing)
      (mp-list-nth thing i)
      (value (subtrie thing i)))
    (clojure.core/nth thing i)))

;; prelude has: reverse, propose1, iterate, replicate, repeat

;; range - overrides original prelude (performance + generalization)

(defn _range [n k]
  (if (gte k n) (mk_nil) (pair k (_range n (add k 1)))))

(define-deterministic-primitive range [n]
  (_range n 0))

;; map - overrides original prelude - BUT DON'T DO THIS.

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
                      (do (clojure.core/assert (metaprob-pair? mp-list) (trie-keys mp-list))
                      (pair (mp-fn (first mp-list))
                            (maplist (rest mp-list))))))]
            (maplist mp-seq))
          ;; tbd: do this with zipmap instead of recursion
          (letfn [(maptup [i]
                    (if (has-subtrie? mp-seq i)
                      (assoc (maptup (+ i 1))
                             i
                             (new-trie (mp-fn (value (subtrie mp-seq i)))))
                      {}))]
            (trie-from-map (maptup 0) "tuple"))))
      (clojure.core/map mp-fn mp-seq))))    ;??? this isn't right

;; original prelude has: imap, zipmap, for_each, filter

;; append - overrides original prelude (performance)
;; This is only for metaprob lists, not for tuples.

(define-deterministic-primitive append [x y]
  (if (metaprob-pair? x)
    (pair (first x) (append (rest x) y))
    y))

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; What about sp = tracing_proposer_to_prob_prog in prelude (!!) - do
;; we need it, how to define, etc.?

;; original prelude has: proposer_of factor apply_with_address

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; error - overrides original prelude (???)

(define-deterministic-primitive error [x]
  (clojure.core/assert false x))                     ;from prelude.vnts

;; capture_tag_address - overrides original prelude - but definition is the same.
;; Compare builtin resolve_tag_address, defined above.

(define-deterministic-primitive capture_tag_address [& stuff]
  stuff)

(defn collection-subtries-to-seq [coll]
  (clojure.core/assert trace? coll)
  (if (has-subtrie? coll rest-marker)
    (letfn [(re [coll]
              (if (empty-trace? coll)
                '()
                (cons coll (re (rest coll)))))]
      (re coll))
    (subtries-to-seq coll)))


;; --------------------
;; Lexical environments, needed by program macro.

(defprotocol IEnv
  "An environment frame"
  (env-lookup [_ name])
  (env-bind [_ name value]))

(deftype TopLevelEnv
    [the-ns]
  IEnv
  (env-lookup [_ name]
    (deref (ns-resolve the-ns (symbol name))))
  (env-bind [_ name value]
    ;; how to create a new binding in a namespace (a la def)???
    (let [sym (symbol name)
          r (ns-resolve the-ns sym value)
          r (if r r (binding [*ns* the-ns]
                      (eval `(def ~sym))))]
      (ref-set r value))))

(defn make-top-level-env [ns]
  (TopLevelEnv. ns))

(deftype Frame
    [the-parent
     bindings-ref]
  IEnv
  (env-lookup [_ name]
    (clojure.core/assert (string? name) [(type name) name])
    (let [bs (deref bindings-ref)]
      (if (contains? bs name)
        (get bs name)
        (env-lookup the-parent name))))
  (env-bind [_ name value]
    (ref-set bindings-ref (assoc (deref bindings-ref) name value))))


;; env_lookup - overrides original prelude

(define-deterministic-primitive env_lookup [env name]
  (clojure.core/assert (satisfies? IEnv env))
  (env-lookup env (str name)))

;; make_env - overrides original prelude

(define-deterministic-primitive make_env [parent]
  (Frame. parent (ref {})))

;; match_bind - overrides original prelude
;; match-bind [env pattern input]

(defn match-bind [pattern input env]
  ;; pattern is a trace (variable or list, I think, maybe seq)
  (clojure.core/assert trace? pattern)
  (case (value pattern)
    "variable" (env-bind env (value (subtrie pattern "name")) input)
    "tuple"
    ;; input is either a metaprob list or a metaprob tuple
    (doseq [p (subtries-to-seq pattern)
            i (metaprob-collection-to-seq input)]
      (match-bind p i env))))

(define-deterministic-primitive match_bind [pattern input env]
  (dosync (match-bind pattern input env)))
