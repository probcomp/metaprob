(ns dontknow.builtin
  (:require [dontknow.trie :refer :all]))

(defn flip [weight] (<= (rand) weight))

(defn uniform [a b] (+ (rand (- b a)) a))

; Metaprob arrays (node with children 0 ... size-1)

(defn mp*array-from-seq [sequ]
  (trie-from-hash-map (zipmap (range (count sequ))
                              (for [member sequ] (new-trie member)))))

; Size of a metaprob array

(defn mp*size [trie]
  (defn size-from [trie i]
    (if (has-subtrie? trie i)
      (+ 1 (size-from trie (+ i 1)))
      0))
  (size-from trie 0))

(defn seq-from-mp*array [mp*array]
  (for [i (range (mp*size mp*array))]
    (value-at mp*array [i])))

; Convert metaprob array to metaprob list ?
(defn array-to-list [a] 0)

; Translated from prelude.vnts.

; Constructors for metaprob-list type

(def mp*nil (new-trie 'nil))

(defn mp*cons [thing mp*list]
  (trie-from-hash-map {:rest mp*list} thing))

; Predicates for metaprob-list type

(defn mp*null? [thing]
  ;; (= mp*list mp*nil)  ??
  (and (trie? thing)
       (has-value? thing)
       (= (value thing) 'nil)
       (not (has-subtrie? thing :rest))))

(defn mp*pair? [thing]
  (and (trie? thing)
       (has-subtrie? thing :rest)))

(defn mp*list? [thing]
  (and (trie? thing)
       (or (has-subtrie? thing :rest)
           (= (value thing) 'nil))))

; Selectors

(defn mp*first [mp*list]
  (value mp*list))

(defn mp*rest [mp*list]
  (subtrie mp*list :rest))

; Higher level operators

(defn mp*list-to-clojure-seq [s]
  (if (mp*null? s)
    []
    (let [[hd tl] s]
      (conj (mp*list-to-clojure-seq tl) hd))))

(defn mp*seq-to-clojure-seq [s]
  (if (trie? s)
    (if (mp*list? s)
      (mp*list-to-clojure-seq s)
      (for [i (range (mp*size s))]
        (value-at s [i])))
    s))

(declare mp*map)

(defn mp*apply [mp*fn mp*seq]
  (apply mp*fn (mp*seq-to-clojure-seq mp*seq)))

(defn mp*list-map [mp*fn mp*list]
  (if (mp*null? mp*list)
    []
    (mp*cons (mp*apply mp*fn (mp*first mp*list))
             (mp*map mp*fn (mp*rest mp*list)))))

(defn mp*array-map [mp*fn mp*array]
  (let [n (mp*size mp*array)
        r (range n)]
    (trie-from-hash-map (zipmap r
                                (for [i r] (mp*apply mp*fn (value-at mp*array [i])))))))

(defn mp*map [mp*fn thing]
  (if (trie? thing)
    (if (mp*list? thing)
      (mp*list-map mp*fn thing)
      (mp*array-map mp*fn thing))
    (for [x thing] (mp*apply mp*fn x))))



