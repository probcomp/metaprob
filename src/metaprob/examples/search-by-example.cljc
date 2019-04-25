(ns metaprob.examples.search-by-example
  (:require [metaprob.examples.cgpm :refer [cgpm-kl-divergence]]))

(defn rowwise-similarity [cgpm latent-vars row-0 row-1 samples]
  (let [kl-divergence-fn (fn [r0 r1] (cgpm-kl-divergence
                                      cgpm
                                      latent-vars
                                      latent-vars
                                      r0
                                      r1
                                      {}
                                      samples))]
    ;; this is the "symmetrized kl"
    (+ (kl-divergence-fn row-0 row-1)
       (kl-divergence-fn row-1 row-0))))

(defn search
  "Given a model, a vector of maps representing a data table, and
  a (possibly sparse) example row, return a list of `(index, score)`
  tuples, where `index` is the index of a row in the data table and
  `score` is a measure of its similarity to the provided example
  row. The returned vector will be sorted by `score`, most similar
  rows first"
  [cgpm rows example latent-vars samples]

  (->> rows
       (map-indexed
        (fn [i r]
          [i (rowwise-similarity cgpm latent-vars example r samples)]))
       (sort-by second)))


(comment



  )
