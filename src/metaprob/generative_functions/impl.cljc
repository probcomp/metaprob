(ns metaprob.generative-functions.impl)

;; Most general way of creating a generative function: provide implementations of its
;; methods. All other ways of creating generative functions boil down, ultimately, to
;; a call to this function.
(defn make-generative-function
  ([run-in-clojure make-constrained-generator]
   (make-generative-function run-in-clojure make-constrained-generator {}))
  ([run-in-clojure make-constrained-generator others]
   (with-meta run-in-clojure (assoc others :make-constrained-generator make-constrained-generator))))
