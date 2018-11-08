(ns metaprob.compound
  (:refer-clojure :exclude [get contains? keys assoc dissoc get-in empty?])
  (:import (clojure.lang IPersistentMap IPersistentVector ISeq)))

(defprotocol MPCompound
  (get [thing key] "Get value associated with `key` in `thing`")
  (contains? [thing key] "Check whether `key` is associated with a value in `thing`")
  (keys [thing] "Returns a list of keys that are contained by `thing`")
  (unbox [thing] "Returns a Clojure version of this data structure")
  (assoc [thing k v] "Returns a version of this data structure with a new key-value pair")
  (dissoc [thing k] "Returns a version of this data structure without key k")
  (representation [thing] "Returns what kind of MP data structure this `thing` is"))

(defn empty? [m]
  (clojure.core/empty? (unbox m)))
  ;(clojure.core/empty? (keys m)))

(defn compound? [x]
  (and (satisfies? MPCompound x) (not (empty? x))))

(defn procedure? [x]
  (fn? x))

(defn to-map [m]
  (cond
    (not (compound? m)) (throw (IllegalArgumentException. "Cannot convert primitives to maps."))
    (= (representation m) :map) m
    true (into {} (map (fn [k] [k (get m k)]) (keys m)))))


; Recursively transforms an AMPData
(defn unbox-all [m]
  (clojure.walk/prewalk
    (fn [x] (if (satisfies? MPCompound x)
              (unbox x)
              x))
    (unbox m)))

(defn listable? [m]
  (and (compound? m)
       (or (= (representation m) :list)
           (= (representation m) :vector)
           (empty? (keys m))
           (and (= (representation m) :map)
                (= (keys m) '(:first :rest))
                (listable? (get m :rest))))))

(defn to-list [m]
  (cond
    (not (satisfies? MPCompound m)) (throw (IllegalArgumentException. "Cannot convert primitives to lists."))
    (= (representation m) :list) m
    (= (representation m) :vector) (seq m)
    (and (= (representation m) :map) (= (keys m) '(:first :rest))) (cons (get m :first) (to-list (get m :rest)))
    (empty? (keys m)) '()
    true (throw (IllegalArgumentException. "Cannot call to-list on a non-listable object."))))

(defn get-in [m adr]
  (if (seq? adr)
    (if (empty? adr) m (recur (get m (first adr)) (rest adr)))
    (get m adr)))


(extend IPersistentMap
  MPCompound
  {:get clojure.core/get,
   :contains? clojure.core/contains?,
   :keys clojure.core/keys,
   :unbox identity,
   :assoc clojure.core/assoc,
   :dissoc clojure.core/dissoc,
   :representation (fn [_] :map)})

(extend IPersistentVector
  MPCompound
  {:get clojure.core/get,
   :contains? clojure.core/contains?,
   :keys (fn [v] (range (count v))),
   :unbox identity,
   :assoc clojure.core/assoc,
   :dissoc clojure.core/dissoc,
   :representation (fn [_] :vector)})

(extend ISeq
  MPCompound
  {:get (fn [l k] (case k :first (first l), :rest (rest l), nil)),
   :contains? (fn [l k] (and (not (empty? l)) (or (= k :first) (= k :rest)))),
   :keys (fn [l] (if (clojure.core/empty? l) '() '(:first :rest))),
   :unbox identity,
   :assoc (fn [l k v] (assoc (to-map l) k v)),
   :dissoc (fn [l k] (dissoc (to-map l) k)),
   :representation (fn [_] :list)})

(extend clojure.lang.Fn
  MPCompound
  {:get (fn [f k] (get (meta f) k)),
   :contains? (fn [f k] (contains? (meta f) k)),
   :keys (fn [f] (keys (meta f))),
   :unbox (fn [f] (if (meta f) (unbox (meta f)) nil)), ; (comp unbox meta),
   :assoc (fn [f k v] (vary-meta f assoc k v)),
   :dissoc (fn [f k] (vary-meta f dissoc k)),
   :representation (fn [f] (representation (meta f)))})

(extend clojure.lang.Atom
  MPCompound
  {:get (fn [a k] (get (deref a) k)),
   :contains? (fn [a k] (contains? (deref a) k)),
   :keys (fn [a] (keys (deref a))),
   :unbox (fn [a] (if (deref a) (unbox (deref a)) (deref a))),
   :assoc (fn [a k v] (assoc (deref a) k v)),
   :dissoc (fn [a k] (dissoc (deref a) k)),
   :representation (fn [a] (representation (deref a)))})

(extend nil
  MPCompound
  {:get (fn [_ _] nil),
   :contains? (fn [_ _] false),
   :keys (fn [_] '()),
   :unbox (fn [_] nil),
   :assoc clojure.core/assoc,
   :dissoc clojure.core/dissoc,
   :representation (fn [_] :list)}) ; should nil be a list, a vector, or...?
