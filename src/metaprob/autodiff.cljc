(ns metaprob.autodiff
  (:refer-clojure :exclude [+ * - / == < > <= >= zero? number? pos? neg?]))

(defn value-with-derivative [tag value derivative]
  "Create an object representing the value of a function and its derivative.

  That is, create an object representing the values of `f(x)` and `f'(x)`,
  where `f` is a differentiable function, `f'` denotes the derivative of `f`,
  and `x` is a point in the domain of `f`

  This type of object is useful in automatic differentiation and is sometimes
  called a _dual number_:
  https://en.wikipedia.org/wiki/Dual_number

  The `tag` field of this object is an identifier used within the
  implementation of automatic differentiation to distinguish between multiple
  \"levels\" of nested differentiation.

  # Fields
  `value`: the value `f(x)`
  `derivative`: the derivative `f'(x)`
  `autodiff/tag`: internal identifier indicating nesting level within the
                  autodiff computation"
  {::tag tag :value value :derivative derivative})

(declare * + - /)

(def bare-number? clojure.core/number?)

(defn lift-real->real
  "Lift a unary operation `f` on numbers to also support `value-with-derivative`s.

  Note that `df-dx` should itself be a lifted function (i.e. must allow its
  argument to be either a bare number or a `value-with-derivative`) that
  computes the derivative of `f` at a point.

  The returned function accepts either a bare number or a
  `value-with-derivative` as its input, and produces an output of the same type
  and the same nesting level (`:autodiff/tag`)."
  [f df-dx]
  (fn new-f [x]
    (if (bare-number? x)
      (f x)
      (value-with-derivative (::tag x)
                             (new-f (:value x))
                             (* (df-dx (:value x)) (:derivative x))))))

(defn lift-real*real->real
  "Like `lift-real->real`, but here the function `f` is a binary operation.

  The argument `df-dx1` is a function that computes the partial derivative of
  `(f x1 x2)` with respect to `x1`.  It takes two arguments, each of which may
  be either a bare number or a `value-with-derivative`; these arguments
  identify the point  at which to evaluate the derivative (often denoted `(x1,
  x2)` in math notation, though this is a name collision).

  Similarly, `df-dx2` computes the partial derivative of `(f x1 x2)` with
  respect to `x2`.
  
  The returned function accepts either a bare number or a
  `value-with-derivative` for each of its arguments, and produces an output...
  TODO(bzinberg) what does it produce."
  [f df-dx1 df-dx2]
  (fn new-f [x1 x2]
    (cond
      (and (bare-number? x1) (bare-number? x2))
      (f x1 x2)
      (or (and (map? x1) (bare-number? x2))
          (and (map? x1) (map? x2) (clojure.core/> (::tag x1) (::tag x2))))
      (value-with-derivative (::tag x1)
                             (new-f (:value x1) x2)
                             (* (df-dx1 (:value x1) x2) (:derivative x1)))
      (or (and (bare-number? x1) (map? x2))
          (and (map? x1) (map? x2) (clojure.core/< (::tag x1) (::tag x2))))
      (value-with-derivative (::tag x2)
                             (new-f x1 (:value x2))
                             (* (df-dx2 x1 (:value x2)) (:derivative x2)))
      (and (map? x1) (map? x2) (= (::tag x1) (::tag x2)))
      (value-with-derivative (::tag x1)
                             (new-f (:value x1) (:value x2))
                             (+ (* (df-dx1 (:value x1) (:value x2)) (:derivative x1))
                                (* (df-dx2 (:value x1) (:value x2)) (:derivative x2)))))))

(defn lift-real*real*real->real
  [f df-dx1 df-dx2 df-dx3]
  (fn new-f [x1 x2 x3]
    (cond
      (and (bare-number? x1) (bare-number? x2) (bare-number? x3))
      (f x1 x2 x3)

      (and (map? x1)
           (or (bare-number? x2)(clojure.core/< (::tag x2) (::tag x1)))
           (or (bare-number? x3) (clojure.core/< (::tag x3) (::tag x1))))
      (value-with-derivative (::tag x1)
                             (new-f (:value x1) x2 x3)
                             (* (df-dx1 (:value x1) x2 x3) (:derivative x1)))

      (and (map? x2)
           (or (bare-number? x1) (clojure.core/< (::tag x1) (::tag x2)))
           (or (bare-number? x3) (clojure.core/< (::tag x3) (::tag x2))))
      (value-with-derivative (::tag x2)
                             (new-f x1 (:value x2) x3)
                             (* (df-dx2 x1 (:value x2) x3) (:derivative x2)))

      (and (map? x3)
           (or (bare-number? x1) (clojure.core/< (::tag x1) (::tag x3)))
           (or (bare-number? x2) (clojure.core/< (::tag x2) (::tag x3))))
      (value-with-derivative (::tag x3)
                             (new-f x1 x2 (:value x3))
                             (* (df-dx3 x1 x2 (:value x3)) (:derivative x3)))

      (and (map? x1) (map? x2) (= (::tag x1) (::tag x2)) (or (bare-number? x3) (clojure.core/< (::tag x3) (::tag x1))))
      (value-with-derivative (::tag x1)
                             (new-f (:value x1) (:value x2) x3)
                             (+ (* (df-dx1 (:value x1) (:value x2) x3) (:derivative x1))
                                (* (df-dx2 (:value x1) (:value x2) x3) (:derivative x2))))

      (and (map? x1) (map? x3) (= (::tag x1) (::tag x3)) (or (bare-number? x2) (clojure.core/< (::tag x2) (::tag x1))))
      (value-with-derivative (::tag x1)
                             (new-f (:value x1) x2 (:value x3))
                             (+ (* (df-dx1 (:value x1) x2 (:value x3)) (:derivative x1))
                                (* (df-dx3 (:value x1) x2 (:value x3)) (:derivative x3))))

      (and (map? x2) (map? x3) (= (::tag x2) (::tag x3)) (or (bare-number? x1) (clojure.core/< (::tag x1) (::tag x2))))
      (value-with-derivative (::tag x2)
                             (new-f x1 (:value x2) (:value x3))
                             (+ (* (df-dx2 x1 (:value x2) (:value x3)) (:derivative x2))
                                (* (df-dx3 x1 (:value x2) (:value x3)) (:derivative x3))))

      (and (map? x1) (map? x2) (map? x3) (= (::tag x1) (::tag x2) (::tag x3)))
      (value-with-derivative (::tag x2)
                             (new-f (:value x1) (:value x2) (:value x3))
                             (+ (* (df-dx1 (:value x1) (:value x2) (:value x3)) (:derivative x1))
                                (* (df-dx2 (:value x1) (:value x2) (:value x3)) (:derivative x2))
                                (* (df-dx3 (:value x1) (:value x2) (:value x3)) (:derivative x3)))))))

;; For functions like + and *, which can take multiple arguments
(defn lift-real-n->real [f df-dx1 df-dx2]
  (fn [& xs]
    (if (nil? xs)
      (f)
      (reduce (lift-real*real->real f df-dx1 df-dx2) xs))))

;; For functions like - and /, where the first argument is special
(defn lift-real-n+1->real [f df-dx df-dx1 df-dx2]
  (fn [& xs]
    (cond (empty? xs) (f)
          (empty? (rest xs)) ((lift-real->real f df-dx) (first xs))
          :else (reduce (lift-real*real->real f df-dx1 df-dx2) xs))))

(defn recursive-unnest-value [x]
  "Recursively unnest the `value` field of a `value-with-derivative`.

  The return value is `(:value (:value (... (:value x))))`, where the number of
  nestings used is the smallest such that the return value is not a map.  In
  particular, if `x` is already not a map, then the return value is `x`
  itself." 
  (if (map? x)
    (recur (:value x))
    x))

(defn shallow-unnest-value [x]
  "Return the `value` field. Acts as the identity if the argument is not a map."
  (if (map? x) (:value x) x)


(defn lift-real-n->boolean [f]
  (fn [& xs] (apply f (map shallow-unnest-value xs))))


(def + (lift-real-n->real clojure.core/+
                          (fn [x1 x2] 1)
                          (fn [x1 x2] 1)))

(def - (lift-real-n+1->real clojure.core/-
                            (fn [x] -1)
                            (fn [x1 x2] 1)
                            (fn [x1 x2] -1)))

(def * (lift-real-n->real clojure.core/*
                          (fn [x1 x2] x2)
                          (fn [x1 x2] x1)))

(declare log)
(def **
  "Power function, `pow(x1, x2) = x1**x2`. The base `x1` must be positive."
  (lift-real*real->real #(Math/pow %1 %2)
                              (fn [x1 x2] (* x2 (** x1 (- x2 1))))
                              (fn [x1 x2] (* (log x1) (** x1 x2)))))

(def / (lift-real-n+1->real clojure.core//
                              (fn [x] (- (/ (Math/pow x 2))))
                              (fn [x1 x2] (/ x2))
                              (fn [x1 x2] (- (/ x1 (** x2 2))))))

(def sqrt (lift-real->real #(Math/sqrt %)
                           (fn [x] (/ 1 (* 2 (sqrt x))))))

(def exp (lift-real->real #(Math/exp %)
                          (fn [x] (exp x))))

(def log (lift-real->real #(Math/log %)
                          (fn [x] (/ x))))

(def log1p (lift-real->real #(Math/log1p %)
                            (fn [x] (/ (+ 1 x)))))

(declare cos)
(def sin (lift-real->real #(Math/sin %)
                          (fn [x] (cos x))))

(def cos (lift-real->real #(Math/cos %)
                          (fn [x] (- (sin x)))))

(def tan (lift-real->real #(Math/tan %)
                          (fn [x] (+ 1 (** (tan x) 2)))))

(def asin (lift-real->real #(Math/asin %)
                           (fn [x] (/ 1 (sqrt (- 1 (** x 2)))))))

(def acos (lift-real->real #(Math/acos %)
                           (fn [x] (- (/ 1 (sqrt (- 1 (** x 2))))))))

(def atan (lift-real->real #(Math/atan %)
                           (fn [x] (/ 1 (+ 1 (* x x))))))

(declare cosh)
(def sinh (lift-real->real #(Math/sinh %)
                           (fn [x] (cosh x))))

(def cosh (lift-real->real #(Math/cosh %)
                           (fn [x] (sinh x))))


(def tanh (lift-real->real #(Math/tanh %)
                           (fn [x] (- 1 (** (tanh x) 2)))))

(def == (lift-real-n->boolean clojure.core/==))

(def < (lift-real-n->boolean clojure.core/<))

(def > (lift-real-n->boolean clojure.core/>))

(def <= (lift-real-n->boolean clojure.core/<=))

(def >= (lift-real-n->boolean clojure.core/>=))

(def zero? (lift-real-n->boolean clojure.core/zero?))

(def pos? (lift-real-n->boolean clojure.core/pos?))

(def neg? (lift-real-n->boolean clojure.core/neg?))

(def number? (lift-real-n->boolean clojure.core/number?))

;; Q: Do we need abs, floor, ceil, min, max?

(def e (atom 0))

;; Helper function used by `diff` and `gradient`.
;; Returns [(f x) (df-dx x)]
(defn forward-mode [apply-2 apply-1 f x x-deriv]
  ;; Based on R6RS-ad, thus doesn't support tangent vector mode
  (swap! e inc)
  (let [y-forward (f (apply-2 (fn [x x-deriv] (value-with-derivative @e x x-deriv)) x x-deriv))]
    (swap! e dec)
    [(apply-1 (fn [y-forward]
                (if (or (not (map? y-forward)) (clojure.core/< (::tag y-forward) @e))
                  y-forward
                  (:value y-forward)))
              y-forward)
     (apply-1 (fn [y-forward]
                (if (or (not (map? y-forward)) (clojure.core/< (::tag y-forward) @e))
                  0
                  (:derivative y-forward)))
              y-forward)]))

;; Our "map" functions just apply `f`.
(defn diff [f]
  (fn [x]
    (second (forward-mode (fn [f x x-deriv] (f x x-deriv))
                          (fn [f y-forward] (f y-forward))
                          f x 1))))

;; To get a gradient, we differentiate w.r.t. each variable.
;; Assumes f's argument is a vector of real numbers.
(defn gradient [f]
  (fn [x]
    (doall (map (fn [i]
           ((diff (fn [xi] (f (assoc x i xi))))
             (nth x i)))
         (range (count x))))))
