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
  (:require [dontknow.trie :refer :all]))

;; Similar to sp_to_prob_prog

(defn make-deterministic-primitive [name fun]
  (letfn [(simulate [args]          ;sp.simulate
            (apply fun args))
          (py-propose [args _intervention _target _output] ;sp.py_propose
            [(apply fun args) 0])]
    (with-meta fun
      {:trace (trie-from-map {"name" (new-trie name)
                              "executable" (new-trie simulate)
                              "custom_interpreter" (new-trie py-propose)
                              "custom_choice_trace" (new-trie py-propose)
                              "custom_proposer" (new-trie py-propose)
                              "custom_choice_tracing_proposer" (new-trie py-propose)}
                             "PROB prog")})))

;; The aux-name is logically unnecessary but helps with debugging.
;; mp-name is the name the primitive has in the metaprob namespace.
;; lib-name is deprecated.

(defmacro ^:private define-deterministic-primitive [lib-name mp-name params & body]
  ;; Ignore lib-name.  Excise it later.
  (let [aux-name (symbol (str mp-name '|primitive))]
    `(do (declare ~mp-name)
         (defn ~aux-name ~params ~@body)
         (def ~mp-name
           (make-deterministic-primitive '~mp-name
                           ~aux-name)))))


;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

(define-deterministic-primitive mp-assert assert [condition complaint]
  (clojure.core/assert condition complaint))

; Used in prelude.vnts:
;   is_metaprob_array  - how to define?

(def not (make-deterministic-primitive 'not clojure.core/not))
(def eq (make-deterministic-primitive 'eq =))

(define-deterministic-primitive neq neq [x y] (clojure.core/not (= x y)))

(def gt (make-deterministic-primitive 'gt >))
(def gte (make-deterministic-primitive 'gte >=))
(def lte (make-deterministic-primitive 'lte <=))
(def lt (make-deterministic-primitive 'lt <))

(declare append)

(define-deterministic-primitive add add [x y]
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

(define-deterministic-primitive mk_nil mk_nil [] (new-trie))                 ; {{ }}

(declare is_pair first rest)

;; addr is a metaprob list. The trie library wants clojure lists.
;; TBD: permit tuple etc. here?

(defn addrify [addr]
  (if (trace? addr)
    (if (is_pair addr)
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
      (if (map? m)
        (if (contains? m :trace)
          (get m :trace)
          x)
        x))))

(define-deterministic-primitive trace_get trace_get [tr] (value (tracify tr)))        ; *e
(define-deterministic-primitive trace_has trace_has [tr] (has-value? (tracify tr)))
(define-deterministic-primitive trace_set trace_set [tr val]            ; e[e] := e
  (set-value! (tracify tr) val))
(define-deterministic-primitive trace_set_at trace_set_at [tr addr val]
  (set-value-at! (tracify tr) (addrify addr) val))
(define-deterministic-primitive trace_set_subtrace_at trace_set_subtrace_at [tr addr sub]
  (set-subtrie-at! (tracify tr) (addrify addr) sub))
(define-deterministic-primitive trace_has_key trace_has_key [tr key] (has-subtrie? (tracify tr) key))
(define-deterministic-primitive trace_subkeys trace_subkeys [tr] (trie-keys (tracify tr)))
(define-deterministic-primitive lookup lookup [tr addr]
  ;; addr might be a metaprobe seq instead of a clojure seq.
  (subtrace-at (tracify tr) (addrify addr)))  ; e[e]


;; Called 'apply' in lisp
;; Same as in metacirc-stub.vnts

(define-deterministic-primitive interpret interpret [proposer inputs intervention-trace]
  ;; proposer is (fn [args _intervention _target _output] ...)
  (if (has-value? intervention-trace)
    (value intervention-trace)
    ;; Discard weight?
    (proposer (subtrie-values-to-seq inputs)
              intervention-trace
              (new-trie)
              (new-trie))))
  

;; def interpret_prim(f, args, intervention_trace):
;;   if intervention_trace.has():
;;     return intervention_trace.get()
;;   else:
;;     return f(metaprob_collection_to_python_list(args))

(define-deterministic-primitive interpret_prim interpret_prim [simulate inputs intervention-trace]
  (if (has-value? intervention-trace)
    (value intervention-trace)
    (simulate inputs)))


(define-deterministic-primitive mp-pprint pprint [x]
  ;; x is a trie.  need to prettyprint it somehow.
  (print (format "[prettyprint %s]\n" x)))

; Other builtins

;; registerBuiltinSP("flip", typed_nr(BernoulliOutputPSP(),
;;   [t.ProbabilityType()], t.BoolType(), min_req_args=0))

(define-deterministic-primitive flip flip [weight] (<= (rand) weight))

(define-deterministic-primitive uniform-continuous uniform-continuous [a b]
  (+ (rand (- b a)) a))

;; TBD (needed by prelude):
;; trace_sites uniform_categorical uniform_continuous

(define-deterministic-primitive resolve_tag_address resolve_tag_address [stuff]
  stuff)

(define-deterministic-primitive name_for_definiens name_for_definiens [pattern]
  (if (symbol? pattern)
    (if (= pattern '_)
      `definiens
      pattern)
    `definiens))

;; pair - not defined in prelude

(def rest-marker "rest")

;; auxiliary - is a trace a metaprob representation of an empty vector
;; or empty list?

(defn empty-trace? [mp-seq]
  (= (trie-count mp-seq) 0))

(define-deterministic-primitive pair pair [thing mp-list]
  (clojure.core/assert (clojure.core/or (empty-trace? mp-list)
                                        (is_pair mp-list)))
  (trie-from-map {rest-marker mp-list} thing))

;; list_to_array - convert metaprob list to metaprob array (tuple)
;; TBD: rewrite functionally

(define-deterministic-primitive list_to_array list_to_array [mp-list]
  (let [arr (mk_nil)]
    (letfn [(r [mp-list n]
              (if (empty-trace? mp-list)
                arr
                (do (set-value-at! arr n (first mp-list))
                    (r (rest mp-list)
                       (+ n 1)))))]
      (r mp-list 0))))

;; list - builtin

(defn seq-to-metaprob-list [things]
  (if (empty? things)
    (mk_nil)
    (pair (clojure.core/first things)
          (seq-to-metaprob-list (clojure.core/rest things)))))

(define-deterministic-primitive mp-list list [& things]
  (seq-to-metaprob-list things))

;; array_to_list - builtin - metaprob array to metaprob list

(define-deterministic-primitive array_to_list array_to_list [arr]
  (if (trie? arr)
    (letfn [(scan [i]
              (if (has-subtrie? arr i)
                (pair (value (subtrie arr i)) (scan (+ i 1)))
                (mk_nil)))]
      (scan 0))
    ;; seqable?
    (apply list arr)))

;; This is an approximation

(define-deterministic-primitive is_metaprob_array is_metaprob_array [x]
  (clojure.core/and (trace? x)
       (clojure.core/not (has-value? x))
       (let [n (trie-count x)]
         (clojure.core/or (= n 0)
             (clojure.core/and (has-subtrie? x 0)
                  (has-subtrie? x (- n 1)))))))

;; dummy, no longer needed

(define-deterministic-primitive dereify_tag dereify_tag [x]
  x)

;; In metaprob, these are strict functions.

(define-deterministic-primitive mp-and and [a b]
  (clojure.core/and a b))

(define-deterministic-primitive mp-or or [a b]
  (clojure.core/or a b))

(define-deterministic-primitive exactly exactly [& body]
  (assert false "what is exactly, exactly?"))

;; ----------------------------------------------------------------------
;; Defined in original prelude (if they are here, then there should be
;; some good reason not to use the prelude version)

;; first - overrides original prelude (performance + generalization)

(define-deterministic-primitive mp-first first [mp-list]
  (if (trace? mp-list)
    (value mp-list)
    (clojure.core/first mp-list)))

;; rest - overrides original prelude (performance + generalization)

(define-deterministic-primitive mp-rest rest [mp-list]
  (if (trace? mp-list)
    (do (clojure.core/assert (is_pair mp-list))
        (subtrie mp-list rest-marker))
    (clojure.core/rest mp-list)))

;; is_pair - overrides original prelude (performance + generalization)

(define-deterministic-primitive is_pair is_pair [x]
  (clojure.core/and (trace? x)
       (has-value? x)
       (has-subtrie? x rest-marker)))

;; length - overrides original prelude (performance + generalization)

(define-deterministic-primitive length length [x]
  (if (trace? x)
    (if (is_pair x)
      (letfn [(scan [x]
                (if (is_pair x)
                  (+ 1 (scan (rest x)))
                  0))]
        (scan x))
      (trie-count x))
    (count x)))

;; drop - use prelude version?

;; last - overrides original prelude (performance + generalization)

(defn ^:private mp-list-last [mp-list]
  (if (is_pair mp-list)
    (let [more (rest mp-list)]
      (if (clojure.core/not (is_pair more))
        (first mp-list)
        (mp-list-last more)))
    mp-list))

(define-deterministic-primitive mp-last last [mp-list]
  (if (trace? mp-list)
    (mp-list-last mp-list)
    (clojure.core/last mp-list)))

;; nth - overrides original prelude (performance + generalization)

(defn mp-list-nth [mp-list i]
  (if (= i 0)
    (first mp-list)
    (mp-list-nth (rest mp-list) (- i 1))))

(define-deterministic-primitive mp-nth nth [thing i]
  (if (trace? thing)
    (if (is_pair thing)
      (mp-list-nth thing i)
      (value (subtrie thing i)))
    (clojure.core/nth thing i)))

;; prelude has: reverse, propose1, iterate, replicate, repeat

;; range - overrides original prelude (performance + generalization)

(defn _range [n k]
  (if (gte k n) (mk_nil) (pair k (_range n (add k 1)))))

(define-deterministic-primitive mp-range range [n]
  (_range n 0))

;; map - overrides original prelude - BUT DON'T DO THIS.

;; Attempt to make type of result be the same as type of input.
;; --> We'll want to use the version from the original prelude so that
;; the traces can propagate through to calls to the function.

(define-deterministic-primitive mp-map mp-map [mp-fn mp-seq]
  ;; Do something - need to thread the trace through.
  (let [mp-seq (tracify mp-seq)]
    (if (trace? mp-seq)
      (if (empty-trace? mp-seq)
        mp-seq
        (if (is_pair mp-seq)
          (letfn [(maplist [mp-list]
                    (clojure.core/assert (trace? mp-list) mp-list)
                    (if (empty-trace? mp-list)
                      mp-list
                      (do (clojure.core/assert (is_pair mp-list) (trie-keys mp-list))
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

(define-deterministic-primitive append append [x y]
  (if (is_pair x)
    (pair (first x) (append (rest x) y))
    y))

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; What about sp = tracing_proposer_to_prob_prog in prelude (!!) - do
;; we need it, how to define, etc.?

;; original prelude has: proposer_of factor apply_with_address

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; error - overrides original prelude (???)

(define-deterministic-primitive error error [x]
  (clojure.core/assert false x))                     ;from prelude.vnts

;; capture_tag_address - overrides original prelude - but definition is the same.
;; Compare builtin resolve_tag_address, defined above.

(define-deterministic-primitive capture_tag_address capture_tag_address [& stuff]
  stuff)

;; Environments

;; env_lookup - overrides original prelude

(define-deterministic-primitive env_lookup env_lookup [env name]
  (clojure.core/assert (string? name) [name (type name)])
  (if (instance? clojure.lang.Namespace env)
    (deref (ns-resolve env (symbol name)))
    (do (clojure.core/assert (map? env) env)
        (clojure.core/or (get (deref (clojure.core/first env)) name)
            (env_lookup (clojure.core/rest env) name)))))

;; make_env - overrides original prelude

(define-deterministic-primitive make_env make_env [parent]
  (cons (ref {}) parent))

;; match_bind - overrides original prelude

(define-deterministic-primitive match_bind match_bind [pattern inputs env]
  (dosync
   (letfn [(mb [pattern inputs]
             (if (clojure.core/not (seqable? pattern))
               (if (clojure.core/not (= pattern '_))
                 ;; in transaction???
                 (ref-set (clojure.core/first env) pattern inputs))
               (if (clojure.core/not (empty? pattern))
                 (do (mb (first pattern) (first inputs))
                     (mb (rest pattern) (rest inputs))))))]
     (mb pattern inputs))
   env))

;; ---- end of prelude ----

;; Does this get used? I don't think so.  Maybe in tests?

(defn seq-to-metaprob-tuple [things]
  (trie-from-seq (clojure.core/map new-trie things)))
