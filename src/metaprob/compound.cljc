(ns metaprob.compound
  (:refer-clojure :exclude [get contains? keys assoc dissoc get-in empty?]))

(defprotocol MPCompound
  (get [thing key] "Get value associated with `key` in `thing`")
  (contains? [thing key] "Check whether `key` is associated with a value in `thing`")
  (keys [thing] "Returns a list of keys that are contained by `thing`")
  (unbox [thing] "Returns a Clojure version of this data structure")
  (single-assoc [thing k v] "Returns a version of this data structure with a new key-value pair")
  (single-dissoc [thing k] "Returns a version of this data structure without key k")
  (representation [thing] "Returns what kind of MP data structure this `thing` is"))

(defn empty? [m]
  (clojure.core/empty? (unbox m)))

(defn raise-illegal-argument
  [message]
  #?(:clj (throw (IllegalArgumentException. message))
     :cljs (ex-info message {})))

;; SLOW: Avoid if possible
(defn compound? [x]
  (and (satisfies? MPCompound x) (not (empty? x))))

(defn procedure? [x]
  (fn? x))

(defn to-map [m]
  (cond
    (not (compound? m)) (raise-illegal-argument "Cannot convert primitives to maps.")
    (= (representation m) :map) m
    true (into {} (map (fn [k] [k (get m k)]) (keys m)))))


;; Recursively transforms an MPData
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
    (not (satisfies? MPCompound m)) (raise-illegal-argument "Cannot convert primitives to lists.")
    (= (representation m) :list) m
    (= (representation m) :vector) (seq m)
    (and (= (representation m) :map) (= (keys m) '(:first :rest))) (cons (get m :first) (to-list (get m :rest)))
    (empty? (keys m)) '()
    true (raise-illegal-argument "Cannot call to-list on a non-listable object.")))

(defn get-in [m adr]
  (if (seq? adr)
    (if (empty? adr) m (recur (get m (first adr)) (rest adr)))
    (get m adr)))

(extend-protocol MPCompound
  #?(:clj clojure.lang.PersistentArrayMap
     :cljs cljs.core/PersistentArrayMap)
  (get [thing key] (clojure.core/get thing key))
  (contains? [thing key] (clojure.core/contains? thing key))
  (keys [thing] (clojure.core/keys thing))
  (unbox [thing] thing)
  (single-assoc [thing k v] (clojure.core/assoc thing k v))
  (single-dissoc [thing k] (clojure.core/dissoc thing k))
  (representation [thing] :map)

  #?(:clj clojure.lang.PersistentVector
     :cljs cljs.core/PersistentVector)
  (get [thing key] (clojure.core/get thing key))
  (contains? [thing key] (clojure.core/contains? thing key))
  (keys [thing] (range (count thing)))
  (unbox [thing] thing)
  (single-assoc [thing k v] (clojure.core/assoc thing k v))
  (single-dissoc [thing k] (clojure.core/dissoc thing k))
  (representation [thing] :vector)

  #?(:clj clojure.lang.PersistentList
     :cljs cljs.core/List)
  (get [l k] (case k :first (first l), :rest (rest l), nil))
  (contains? [l k] (and (not (empty? l)) (or (= k :first) (= k :rest))))
  (keys [l] (if (clojure.core/empty? l) '() '(:first :rest)))
  (unbox [thing] thing)
  (single-assoc [l k v] (single-assoc (to-map l) k v))
  (single-dissoc [l k] (single-dissoc (to-map l) k))
  (representation [thing] :list)


  #?(:clj clojure.lang.IFn
     :cljs function)
  (get [f k] (get (meta f) k))
  (contains? [f k] (contains? (meta f) k)) ; zs: use metaprob.compound/keys?
  (keys [f] (keys (meta f)))
  (unbox [f] (if (meta f) (unbox (meta f)) nil)) ; (comp unbox meta)
  (single-assoc [f k v] (vary-meta f single-assoc k v))
  (single-dissoc [f k] (vary-meta f single-dissoc k))
  (representation [f] (representation (meta f)))

  #?(:clj clojure.lang.Atom
     :cljs cljs.core/Atom)
  (get [a k] (get (deref a) k)),
  (contains? [a k] (contains? (deref a) k)),
  (keys [a] (keys (deref a))),
  (unbox [a] (if (deref a) (unbox (deref a)) (deref a))),
  (single-assoc [a k v] (single-assoc (deref a) k v)),
  (single-dissoc [a k] (single-dissoc (deref a) k)),
  (representation [a] (representation (deref a)))

  nil
  (get [_ _] nil)
  (contains? [_ _] false)
  (keys [_] '())
  (unbox [_] nil)
  (single-assoc [thing k v] (clojure.core/assoc thing k v))
  (single-dissoc [thing k] (clojure.core/dissoc thing k))
  (representation [_] :list)) ; should nil be a list, a vector, or...?

#?(:clj (extend-protocol MPCompound
          clojure.lang.PersistentList$EmptyList
          (get [l k] (case k :first (first l), :rest (rest l), nil))
          (contains? [l k] (and (not (empty? l)) (or (= k :first) (= k :rest))))
          (keys [l] (if (clojure.core/empty? l) '() '(:first :rest)))
          (unbox [thing] thing)
          (single-assoc [l k v] (single-assoc (to-map l) k v))
          (single-dissoc [l k] (single-dissoc (to-map l) k))
          (representation [thing] :list)

          clojure.lang.LazySeq
          (get [l k] (case k :first (first l), :rest (rest l), nil))
          (contains? [l k] (and (not (empty? l)) (or (= k :first) (= k :rest))))
          (keys [l] (if (clojure.core/empty? l) '() '(:first :rest)))
          (unbox [thing] thing)
          (single-assoc [l k v] (single-assoc (to-map l) k v))
          (single-dissoc [l k] (single-dissoc (to-map l) k))
          (representation [thing] :list)

          clojure.lang.ArraySeq
          (get [l k] (case k :first (first l), :rest (rest l), nil))
          (contains? [l k] (and (not (empty? l)) (or (= k :first) (= k :rest))))
          (keys [l] (if (clojure.core/empty? l) '() '(:first :rest)))
          (unbox [thing] thing)
          (single-assoc [l k v] (single-assoc (to-map l) k v))
          (single-dissoc [l k] (single-dissoc (to-map l) k))
          (representation [thing] :list)

          clojure.lang.Cons
          (get [l k] (case k :first (first l), :rest (rest l), nil))
          (contains? [l k] (and (not (empty? l)) (or (= k :first) (= k :rest))))
          (keys [l] (if (clojure.core/empty? l) '() '(:first :rest)))
          (unbox [thing] thing)
          (single-assoc [l k v] (single-assoc (to-map l) k v))
          (single-dissoc [l k] (single-dissoc (to-map l) k))
          (representation [thing] :list))
   :cljs (extend-protocol MPCompound
           cljs.core/List
           (get [l k] (case k :first (first l), :rest (rest l), nil))
           (contains? [l k] (and (not (empty? l)) (or (= k :first) (= k :rest))))
           (keys [l] (if (clojure.core/empty? l) '() '(:first :rest)))
           (unbox [thing] thing)
           (single-assoc [l k v] (single-assoc (to-map l) k v))
           (single-dissoc [l k] (single-dissoc (to-map l) k))
           (representation [thing] :list)))

(defn assoc
  ([m k v] (single-assoc m k v))
  ([m k v & kvs]
   (let [ret (single-assoc m k v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (raise-illegal-argument "assoc expects an even number of arguments after the collection; found odd number"))
       ret))))

(defn dissoc
  ([m k] (single-dissoc m k))
  ([m k & ks]
   (let [ret (single-dissoc m k)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))
