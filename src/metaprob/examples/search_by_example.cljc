(ns metaprob.examples.search-by-example
  (:require [metaprob.examples.cgpm :refer [cgpm-logpdf]]
            [metaprob.examples.nyt :refer [census-cgpm generate-census-row
                                           cluster-data cluster-count]]))

(defn kli [p_in q_in]
  (let [p (if (<= p_in 0) 0.00000000000000000001 p_in)
        q (if (<= q_in 0) 0.00000000000000000001 q_in)]
    (* p (Math/log (/ p q)))))

(defn kl [ps qs]
  (apply +
         (map (fn [p q] (kli p q))
              ps qs)))

(defn make-ex-row []
  (zipmap [:percent_married_children
           :percent_black
           :percent_college
           :percap]
          (generate-census-row)))

(defn probability-for-cluster-assignments [cgpm row emphasis]
  (let [probs (map (fn [ci]
                      (Math/exp
                       (cgpm-logpdf cgpm
                                    {emphasis ci}
                                    row
                                    {})))
                    (range cluster-count))]
    probs))

(def pfca-memo (memoize probability-for-cluster-assignments))

(defn rowwise-similarity [cgpm row-0 row-1 emphasis]
  (kl
   (time (doall (probability-for-cluster-assignments cgpm row-0 emphasis)))
   (time (doall (probability-for-cluster-assignments cgpm row-1 emphasis)))))

(defn search
  [cgpm rows example emphasis]
  (->> rows
       (map-indexed
        (fn [i r]
          (println "row.")
          [i (rowwise-similarity cgpm example r emphasis)]))
       (sort-by second)))

(comment
  (search
   census-cgpm
   [(make-ex-row)
    (make-ex-row)]
   {:percent_black 0.10}
   :cluster-for-percap))
