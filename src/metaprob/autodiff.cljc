(ns metaprob.autodiff
  (:refer-clojure :exclude [+ * - / == < > <= >= zero? number? pos? neg?]))

;; Dual numbers represent computations dependent on a value with respect to which we are differentiating.
;; dual contains the current derivative; primal contains the current value; and tag is a special tag
;; that helps distinguish between multiple "levels" of nested differentiations.
(defn make-dual-number [tag primal dual]
  {:tag tag :primal primal :dual dual})

(declare * + - /)

(def non-dual? clojure.core/number?)

;; Take a unary operation f on numbers
;; and lift it to work with dual numbers.
;; Note that df-dx should itself be a lifted
;; function for computing the derivative of f
;; at a point.
(defn lift-real->real
  [f df-dx]
  (fn new-f [x]
    (if (map? x)
      (make-dual-number (:tag x)
                        (new-f (:primal x))
                        (* (df-dx (:primal x)) (:dual x)))
      (f x))))

;; Lift a binary operation on numbers to work
;; with dual numbers. We have to consider several
;; cases here.
(defn lift-real*real->real
  [f df-dx1 df-dx2]
  (fn new-f [x1 x2]
    (cond
      (and (non-dual? x1) (non-dual? x2))
      (f x1 x2)
      (or (and (map? x1) (non-dual? x2))
          (and (map? x1) (map? x2) (clojure.core/> (:tag x1) (:tag x2))))
      (make-dual-number (:tag x1)
                        (new-f (:primal x1) x2)
                        (* (df-dx1 (:primal x1) x2) (:dual x1)))
      (or (and (non-dual? x1) (map? x2))
          (and (map? x1) (map? x2) (clojure.core/< (:tag x1) (:tag x2))))
      (make-dual-number (:tag x2)
                        (new-f x1 (:primal x2))
                        (* (df-dx2 x1 (:primal x2)) (:dual x2)))
      (and (map? x1) (map? x2) (= (:tag x1) (:tag x2)))
      (make-dual-number (:tag x1)
                        (new-f (:primal x1) (:primal x2))
                        (+ (* (df-dx1 (:primal x1) (:primal x2)) (:dual x1))
                           (* (df-dx2 (:primal x1) (:primal x2)) (:dual x2)))))))

(defn lift-real*real*real->real
  [f df-dx1 df-dx2 df-dx3]
  (fn new-f [x1 x2 x3]
    (cond
      (and (non-dual? x1) (non-dual? x2) (non-dual? x3))
      (f x1 x2 x3)

      (and (map? x1)
           (or (non-dual? x2)(clojure.core/< (:tag x2) (:tag x1)))
           (or (non-dual? x3) (clojure.core/< (:tag x3) (:tag x1))))
      (make-dual-number (:tag x1)
                        (new-f (:primal x1) x2 x3)
                        (* (df-dx1 (:primal x1) x2 x3) (:dual x1)))

      (and (map? x2)
           (or (non-dual? x1) (clojure.core/< (:tag x1) (:tag x2)))
           (or (non-dual? x3) (clojure.core/< (:tag x3) (:tag x2))))
      (make-dual-number (:tag x2)
                        (new-f x1 (:primal x2) x3)
                        (* (df-dx2 x1 (:primal x2) x3) (:dual x2)))

      (and (map? x3)
           (or (non-dual? x1) (clojure.core/< (:tag x1) (:tag x3)))
           (or (non-dual? x2) (clojure.core/< (:tag x2) (:tag x3))))
      (make-dual-number (:tag x3)
                        (new-f x1 x2 (:primal x3))
                        (* (df-dx3 x1 x2 (:primal x3)) (:dual x3)))

      (and (map? x1) (map? x2) (= (:tag x1) (:tag x2)) (or (non-dual? x3) (clojure.core/< (:tag x3) (:tag x1))))
      (make-dual-number (:tag x1)
                        (new-f (:primal x1) (:primal x2) x3)
                        (+ (* (df-dx1 (:primal x1) (:primal x2) x3) (:dual x1))
                           (* (df-dx2 (:primal x1) (:primal x2) x3) (:dual x2))))

      (and (map? x1) (map? x3) (= (:tag x1) (:tag x3)) (or (non-dual? x2) (clojure.core/< (:tag x2) (:tag x1))))
      (make-dual-number (:tag x1)
                        (new-f (:primal x1) x2 (:primal x3))
                        (+ (* (df-dx1 (:primal x1) x2 (:primal x3)) (:dual x1))
                           (* (df-dx3 (:primal x1) x2 (:primal x3)) (:dual x3))))

      (and (map? x2) (map? x3) (= (:tag x2) (:tag x3)) (or (non-dual? x1) (clojure.core/< (:tag x1) (:tag x2))))
      (make-dual-number (:tag x2)
                        (new-f x1 (:primal x2) (:primal x3))
                        (+ (* (df-dx2 x1 (:primal x2) (:primal x3)) (:dual x2))
                           (* (df-dx3 x1 (:primal x2) (:primal x3)) (:dual x3))))

      (and (map? x1) (map? x2) (map? x3) (= (:tag x1) (:tag x2) (:tag x3)))
      (make-dual-number (:tag x2)
                        (new-f (:primal x1) (:primal x2) (:primal x3))
                        (+ (* (df-dx1 (:primal x1) (:primal x2) (:primal x3)) (:dual x1))
                           (* (df-dx2 (:primal x1) (:primal x2) (:primal x3)) (:dual x2))
                           (* (df-dx3 (:primal x1) (:primal x2) (:primal x3)) (:dual x3)))))))

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

(defn primal [x]
  (if (non-dual? x) x (:primal x)))


(defn lift-real-n->boolean [f]
  (fn [& xs] (apply f (map primal xs))))


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
(def ** (lift-real*real->real #(Math/pow %1 %2)
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

;; Helper function used by `diff` and `gradient-vector`.
;; Returns [(f x) (df-dx x)]
(defn forward-mode [map-independent map-dependent f x x-perturbation]
  ;; Based on R6RS-ad, thus doesn't support tangent vector mode
  (swap! e inc)
  (let [y-forward (f (map-independent (fn [x x-dual] (make-dual-number @e x x-dual))
                                      x
                                      x-perturbation))]
    (swap! e dec)
    [(map-dependent (fn [y-forward]
                      (if (or (not (map? y-forward))
                              (clojure.core/< (:tag y-forward) @e))
                        y-forward
                        (:primal y-forward)))
                    y-forward)
     (map-dependent (fn [y-forward]
                      (if (or (not (map? y-forward))
                              (clojure.core/< (:tag y-forward) @e))
                        0
                        (:dual y-forward)))
                    y-forward)]))

;; Our "map" functions just apply `f`.
(defn diff [f]
  (fn [x]
    (second (forward-mode (fn [f x x-dual] (f x x-dual))
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
