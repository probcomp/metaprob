(ns metaprob.trace
  (:require [clojure.string :as string])
  (:require [metaprob.mutable-trace :as mut]))

;; empty-trace? - is this trace a metaprob representation of an empty tuple/list?

(defn empty-trace? [x]
  (let [x (proper-trace x)]
    (and x
         (= (trace-count x) 0)
         (not (has-value? x)))))


;; Convert a value to be used as a key to a pure clojure value so that
;; hash and = will work on it.

(defn purify [x]
  (if (mutable-trace? x)
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


;;    (assert (metaprob-value? val) ["storing non-metaprob value" val])

;;   (assert (metaprob-value? val)
;;           ["initial value is non-metaprob" val (environment? val) (type val)])

;;   (assert (metaprob-value? val) ["starting value is a non-metaprob value" val])

;;    (assert (acceptable? key sub)
;;            ["unacceptable assignment" _ (reason-unacceptable key sub)])

;; trie-from-map
;;  (doseq [[key sub] (seq maap)]
;;    (assert (acceptable? key sub)
;;            ["bad subtrie assignment" (reason-unacceptable key sub)]))

(defn environment? [x] (= (type x) (type *ns*)))

;; Useable as a subtrace key

(defn ok-key? [val]
  (or (number? val)
      (string? val)
      (boolean? val)
      (= val nil)      ; needed?
      (= val '())      ; needed?
      (list? val)      ; from detracify
      (vector? val)    ; from detracify
      (map? val)))     ; from detracify

;; Storable as a value

(defn metaprob-value? [val]
  (or (ok-key? val)
      (mutable-trace? val)                      ;possibly a locative
      (environment? val)
      (and (instance? clojure.lang.IFn val)
           ;; ugh
           (not (seq? val))
           (not (symbol? val)))))

;; Is key acceptable for use in storing sub as a subtrie?

(defn acceptable? [key sub]
  ;; Really stupid type system.  Functions should be stored, and only
  ;; be stored, under the "foreign-generate" or "foreign-query" property.
  (if (ok-key? key)
    (if (trie? sub)
      (if (has-value? sub)
        (let [val (value sub)]
          (if (string/starts-with? key "foreign-")
            (or (instance? clojure.lang.IFn val)
                (meta val))
            (metaprob-value? val)))
        true)
      false)
    false))

;; This is for debugging

(defn reason-unacceptable [key sub]
  (if (ok-key? key)
    (if (trie? sub)
      (if (has-value? sub)
        (let [val (value sub)]
          (if (string/starts-with? key "foreign-")
            (if (instance? clojure.lang.IFn val)
              ["acceptable - executable IFn" key sub val]
              (if (meta val)
                ["acceptable - executable meta" key sub val]
                ["not IFn and not meta" key sub val]))
            (if (metaprob-value? val)
              ["acceptable" key sub val]
              ["not a metaprob value" key sub val])))
        ["acceptable - no sub-value" key sub])
      ["subtrie is a non-trie" key sub (type sub)])
    ["not to be used as a key" key sub]))


;; -----------------------------------------------------------------------------

;; OK as an argument to tracify?

(defn tracish? [x]
  (or (mutable-trace? x)
      (let [m (meta x)]
        (and (map? m) (contains? m :trace)))))

(defn trace? [x]
  (or (tracish? x)
      (seq? x)     ;; clojure lists are seqs
      (vector? x)
      (map? x)))

