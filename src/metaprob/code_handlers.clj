(ns metaprob.code-handlers)

(defn name-checker [n]
  (fn [x]
    (and (seq? x)
         (symbol? (first x))
         (= (name (first x)) n))))

(defn symbol-checker [n]
  (fn [x]
    (and (seq? x)
         (= (first x) n))))

(def gen-expr? (name-checker "gen"))

(defn gen-name
  [expr]
  (cond
    (symbol? (second expr)) (second expr)
    (map? (second expr)) (get (second expr) :name)
    true nil))

(defn gen-annotations
  [expr]
  (if (map? (second expr))
    (second expr)
    {}))

(defn gen-transformation
  [expr]
  (if (map? (second expr))
    (get (second expr) :transform) nil))

(defn gen-tracer-name
  [expr]
  (if (map? (second expr))
    (get (second expr) :tracing-with) nil))

(defn gen-has-annotations?
  [expr]
  (not (vector? (second expr))))

(defn gen-pattern
  [expr]
  (if (gen-has-annotations? expr)
    (nth expr 2)
    (second expr)))

(defn gen-body
  [expr]
  (if (gen-has-annotations? expr)
    (rest (rest (rest expr)))
    (rest (rest expr))))

(defn map-gen
  [f gen-expr]
  (if (gen-has-annotations? gen-expr)
    (cons (first gen-expr)
          (cons (second gen-expr)
                (cons (gen-pattern gen-expr) (map f (gen-body gen-expr)))))
    (cons (first gen-expr)
          (cons (gen-pattern gen-expr) (map f (gen-body gen-expr))))))

(def if-expr? (symbol-checker 'if))

(def if-predicate second)

(defn if-then-clause [expr] (nth expr 2))

(defn if-else-clause [expr]
  (if (< (count expr) 4) nil (nth expr 3)))

(def variable? symbol?)

(def quote-expr? (symbol-checker 'quote))
(def quote-quoted second)

(defn literal?
  [expr]
  (or (not (or (seq? expr) (vector? expr) (map? expr)))
      (empty? expr)))