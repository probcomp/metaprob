(ns metaprob.examples.search-by-example
  (:require [metaprob.examples.cgpm :refer [cgpm-logpdf]]
            [metaprob.examples.nyt :refer [census-cgpm generate-census-row
                                           cluster-data cluster-count]]
            [metaprob.prelude :as prelude]
            [metaprob.trace :as trace]
            [metaprob.examples.data :refer [nyt-data]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            ))

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
                       (p :logpdf
                          (cgpm-logpdf
                           cgpm
                           {emphasis ci}
                           row
                           {}))))
                    (range cluster-count))]
    probs))


(defn row-trace [tr row]
  (reduce
   (fn [t r]
     (trace-set-value t )
     )
   tr
   row))

(let [[_ tr _] (prelude/infer-and-score
                :procedure (:proc  census-cgpm))]
  (trace/addresses-of tr))

(second (prelude/infer-and-score
         :procedure (:proc  census-cgpm)))


(defn probability-distribution-on-cluster
  [model row view]
  (let [;; IRL we'd find only the data in row which intersect with
        ;; columns in view. In this case, we have only one view so
        ;; we'll skip.

        ;; First, constrain the row with the information we have.  The
        ;; thing is... I think we can just use `row` here, since the
        ;; trace is the same shape?
        constrained-trace    view ;; (construct-target  subset-row {})

        ;; Get the possible cluster assignments given the view.
        cluster-assignements (view-cluster-indices-mapping view)

        ;; Define a a function that scores a trace given a specific
        ;; cluster assignment.
        assign-and-score-cluster (fn [cluster-assignment]
                                   (let [assigned-trace (set-cluster-assignment
                                                         constrained-trace
                                                         view
                                                         cluster-assignment)
                                         [_ _ score] (infer
                                                      :procedure model
                                                      :target-trace assigned-trace)]
                                     (exp score)))

        ;; Take the defined function and score all the cluster
        ;; assigments in the given context.
        unnormalized-scores (map assign-and-score-cluster cluster-assignements)]

    ;; The score is un-normalized -- but we know that it has to be
    ;; assigned to some cluster -- thus the scores need to sum to one.
    (normalize unnormalized-scores)))



(defn rowwise-similarity [cgpm example-p-f-c-a row-1 emphasis]
  (kl
   example-p-f-c-a
   (p :p-f-c-a (probability-for-cluster-assignments cgpm row-1 emphasis))))

(defn search
  [cgpm rows example emphasis]
  (let [example-p-f-c-a (probability-for-cluster-assignments cgpm example emphasis)]
      (->> rows
           (map-indexed
            (fn [i r]
              [i (rowwise-similarity cgpm example-p-f-c-a r emphasis)]))
           (sort-by second))))

(comment
  (tufte/add-basic-println-handler! {})

  (defn fix-table [t]
    (map
     #(dissoc % :geo_fips :district_name)
     (clojure.walk/keywordize-keys t)))

  (profile {} (p :search (search
                          census-cgpm
                          (fix-table nyt-data)
                          {:percent_black 0.10}
                          :cluster-for-percap)))


  )
