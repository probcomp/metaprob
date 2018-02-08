(ns metaprob.environment)

;; --------------------
;; Lexical environments, needed by program macro.

(defprotocol IEnv
  "An environment frame"
  (env-lookup [_ name])
  (env-bind! [_ name value]))

(defn environment? [x] (satisfies? IEnv x))

(deftype TopLevelEnv
    [the-ns]
  IEnv
  (env-lookup [_ name]
    (let [v (ns-resolve the-ns (symbol name))]
      (assert (var? v) ["not bound" name the-ns])
      (assert (not (get (meta v) :macro)) ["reference to macro" name the-ns])
      (deref v)))
  (env-bind! [_ name value]
    ;; how to create a new binding in a namespace (a la def)???
    (let [sym (symbol name)
          r (ns-resolve the-ns sym value)
          r (if r r (binding [*ns* the-ns]
                      (eval `(def ~sym))))]
      (ref-set r value)
      nil)))

(defn make-top-level-env [ns]
  (TopLevelEnv. (if (symbol? ns)
                    (find-ns ns)
                    ns)))

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
  (env-bind! [_ name value]
    (ref-set bindings-ref (assoc (deref bindings-ref) name value))
    nil))

(defn make-sub-environment [parent]
  (clojure.core/assert (satisfies? IEnv parent))
  (Frame. parent (ref {})))
