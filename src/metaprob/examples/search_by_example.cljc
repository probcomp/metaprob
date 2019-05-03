(ns metaprob.examples.search-by-example
  (:require [metaprob.examples.cgpm :refer [cgpm-logpdf]]
            [metaprob.examples.nyt :as nyt :refer [generate-census-row]]
            [metaprob.examples.multimixture-dsl :as mmix]
            [metaprob.prelude :as prelude]
            [metaprob.trace :as trace]
            [metaprob.examples.data :as data]
            [metaprob.examples.pfcas :as pfcas]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(defn- kli [p-in q-in]
  (assert (<= 0 p-in 1))
  (assert (<= 0 q-in 1))
  (let [p (max 1E-300 p-in)
        q (max 1E-300 q-in)]
    (* p (let [x (Math/log (/ p q))]
           (println p-in q-in x)
           x))))

(defnp kl [ps qs]
  "K-L divergence between two vectors of floating point numbers."
  (apply +
         (map (fn [p q] (kli p q))
              ps
              qs)))

(defn symmetrized-kl
  [ps qs]
  (+ (kl ps qs)
     (kl qs ps)))

(defnp constrain-by-row
  "Constrains the given trace such that the values chosen for each column are the
  ones in the provided row."
  [trace row]
  (reduce (fn [trace [column value]]
            (trace/trace-set-value trace column value))
          trace
          row))

(defnp constrain-by-view
  "Constrains the given trace with such that the view chosen is the one provided."
  [trace cluster view]
  (trace/trace-set-value trace (mmix/view-cluster-address view) cluster))

(defnp constrain-by-cluster
  "Constrains the given trace such that the cluster chosen is the one provided."
  [trace cluster columns]
  (reduce (fn [trace column]
            (trace/trace-set-value trace
                                   (mmix/column-cluster-address column)
                                   cluster))
          trace
          columns))

(defnp normalize
  "Normalizes a vector of numbers such that they sum to 1."
  [ns]
  (let [sum (apply + ns)]
    (mapv #(/ % sum)
          ns)))

(defnp probability-distribution-on-cluster
  [model row view]
  (let [columns (-> nyt/clusters second keys)
        cluster-addresses (range (nyt/cluster-count nyt/clusters))]
    (->> cluster-addresses
         (map (fn [cluster-address]
                (let [trace (-> {}
                                (constrain-by-view cluster-address view)
                                (constrain-by-cluster cluster-address columns)
                                (constrain-by-row row))
                      [_ _ score] (prelude/infer-and-score :procedure model
                                                           :observation-trace trace)]
                  (prelude/exp score))))
         (normalize))))

(defnp rowwise-similarity [cgpm view example-pfca row emphasis]
  (kl example-pfca (probability-distribution-on-cluster cgpm row view)))

(defnp search
  [cgpm rows example emphasis]
  (let [view (mmix/view-for-column emphasis)
        example-pfca (probability-distribution-on-cluster (:proc cgpm) example emphasis)]
    (->> rows
         (map-indexed (fn [index row]
                        [index (rowwise-similarity (:proc cgpm) view example-pfca row emphasis)]))
         (sort-by second))))

(defnp cached-search
  [cgpm example emphasis]
  (let [view (mmix/view-for-column emphasis)
        example-pfca (probability-distribution-on-cluster (:proc cgpm) example emphasis)]
    (->> pfcas/pfcas
         (map-indexed (fn [index pfca]
                        [index (symmetrized-kl example-pfca pfca)]))
         (sort-by second))))

#?(:clj (defn save-pfcas
          [filename model rows emphasis]
          (let [data (mapv #(probability-distribution-on-cluster model % emphasis)
                           rows)

                sexp (with-out-str
                       (pr '(ns metaprob.examples.pfcas))
                       (pr `(~'def ~'pfcas ~data)))]
            (spit filename sexp))))

(comment

  (let [fix-table (fn fix-table [t]
                    (map
                     #(dissoc % "geo_fips" "district_name")
                     t))]
    (save-pfcas "pfcas.cljc"
                (:proc nyt/census-cgpm)
                (fix-table data/nyt-data)
                "percent_black"))

  (cached-search nyt/census-cgpm
                 {"percent_black" 0.90}
                 "cluster-for-percap")

  (prelude/infer-and-score :procedure (:proc nyt/census-cgpm))

  (:proc nyt/census-cgpm)

  ((:proc nyt/census-cgpm))

  (probability-distribution-on-cluster (:proc nyt/census-cgpm)
                                       {:percent_black 0.1}
                                       :percent_black)

  (search nyt/census-cgpm
          data/nyt-data
          {:percent_black 0.1}
          :percent_black)

  )
