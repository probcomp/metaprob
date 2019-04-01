(ns metaprob.examples.spelling-correction
  (:refer-clojure :exclude [map replicate apply])
  (:require [metaprob.trace :refer :all]
            [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer [map expt replicate]]
            [clojure.pprint :refer [pprint]]
            [metaprob.distributions :refer :all]
            [clojure.string :refer [index-of]]
            [metaprob.inference :refer :all]))

;; A list of all English letters
(def alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                 "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

;; Washington State cities:
(def washington-cities {"aberdeen" 16255, "bellingham" 83365, "bremerton" 38572, "everett" 106736, "richland" 53019,
                        "seattle" 668342, "spokane" 212052, "tacoma" 205159, "vancouver" 169294, "yakima" 93357})

;; Helper functions for string manipulation
(defn delete-at-index [s idx] (str (subs s 0 idx) (subs s (+ 1 idx))))
(defn insert-at-index [s idx c] (str (subs s 0 idx) c (subs s idx)))
(defn replace-at-index [s idx c] (str (subs s 0 idx) c (subs s (+ 1 idx))))

;; Introduce a single typo to a string
(def add-error
  (gen [true-string]
    (let-traced [error-type (uniform-discrete [:deletion :insertion :substitution])
                 highest-possible-index (+ (count true-string) (if (= error-type :insertion) 1 0))
                 error-index (uniform-discrete (range highest-possible-index))
                 new-letter (when (not= error-type :deletion) (uniform-discrete alphabet))]
      (cond
        (= error-type :deletion) (delete-at-index true-string error-index)
        (= error-type :insertion) (insert-at-index true-string error-index new-letter)
        (= error-type :substitution) (replace-at-index true-string error-index new-letter)))))

;; Introduce zero or more typos to a string
(def add-errors-model
  (gen [true-string]
    (let [helper (fn [current-string error-number]
                   (if (at `(~error-number "done-adding-errors?") flip 0.9)
                     (at "final-string" exactly current-string)
                     (recur (at error-number add-error current-string) (+ error-number 1))))]
      (helper true-string 0))))

;; Heuristics
(defn distance-from-index
  [s idx c]
  (min
    (+ 1 (or (index-of (clojure.string/reverse (subs s 0 (min (count s) idx))) c) (count s)))
    (or (index-of (subs s (min (count s) idx)) c) (+ (count s) 1))))

;; Probability, in the guide program, of adding this letter at this index
(defn probability-of-letter
  [l observed-string error-index]
  (expt 0.01 (distance-from-index observed-string error-index l)))

;; Probabilities with which we will choose each letter for insertion at this index
(defn letter-probabilities
  [observed-string error-index]
  (zipmap
    alphabet
    (map #(probability-of-letter % observed-string error-index) alphabet)))

(def should-probably-add-typo? not=)

(defn preferred-error-type
  [current-string observed-string]
  (cond
    (< (count current-string) (count observed-string)) :insertion
    (> (count current-string) (count observed-string)) :deletion
    (= (count current-string) (count observed-string)) :substitution))


(defn good-index-for-error?
  [i current-string observed-string]
  (or (>= i (count observed-string))
      (<= (count current-string) i)
      (not= (subs current-string i (+ i 1)) (subs observed-string i (+ i 1)))))

(defn score-error-type
  [error-type current-string observed-string]
  (if (= error-type (preferred-error-type current-string observed-string))
    1 0.05))

(defn make-smart-add-error
  [observed-string]
  (gen [current-string]
    (let-traced [;; Error types:
                 error-types
                 [:insertion :deletion :substitution]

                 error-type-probabilities
                 (zipmap
                   error-types
                   (map #(score-error-type % current-string observed-string) error-types))

                 error-type
                 (categorical error-type-probabilities)

                 ;; Error index
                 error-index-upper-bound
                 (+ (count current-string) (if (= error-type :insertion) 1 0))

                 index-probs
                 (map
                   (fn [i] (if (good-index-for-error? i current-string observed-string) 1 0.02))
                   (range error-index-upper-bound))

                 error-index
                 (categorical index-probs)

                 ;; Letter to introduce
                 letter-probs
                 (letter-probabilities observed-string error-index)

                 new-letter
                 (if (= error-type :deletion) "" (categorical letter-probs))]

      ;; Apply the typo
      (cond
        (= error-type :deletion) (delete-at-index current-string error-index)
        (= error-type :insertion) (insert-at-index current-string error-index new-letter)
        (= error-type :substitution) (replace-at-index current-string error-index new-letter)))))

(defn make-add-errors-proposer
  [observation-trace]
  (let [observed-string (trace-value observation-trace "final-string")
        smart-add-error (make-smart-add-error observed-string)]

    ;; This is the "smart add-errors"
    (gen [true-string]
      (let [helper
            (fn [current-string i]
              (let [stop-prob (if (should-probably-add-typo? current-string observed-string) 0.000 0.999)]
                (if (at `(~i "done-adding-errors?") flip stop-prob)
                  ;; If we are done adding errors, just return the current string
                  (at "final-string" exactly current-string)
                  ;; Otherwise, add another typo and loop
                  (recur (at i smart-add-error current-string)
                         (+ i 1)))))]

        ;; Start the process going
        (helper true-string 0)))))


(def add-errors
  (with-custom-proposal-attached
    add-errors-model
    make-add-errors-proposer
    (fn [tr] (trace-has-value? tr "final-string"))))


(defn add-errors-demo []
  (println "Importance resampling with custom proposal")
  (pprint (importance-resampling-custom-proposal
    :model add-errors-model
    :proposer make-add-errors-proposer
    :inputs ["computer"]
    :observation-trace {"final-string" {:value "camptr"}}
    :n-particles 30))
  (println "Importance resampling with internal proposal")
  (pprint (importance-resampling
            :model add-errors
            :inputs ["computer"]
            :observation-trace {"final-string" {:value "camptr"}}
            :n-particles 30)))



;; Choose a city and maybe add typos to it
(def spelling-model
  (gen []
    (let-traced [true-city (categorical washington-cities)
                 with-typos (add-errors true-city)]
      with-typos)))

(defn spelling-model-demo []
  ;; Inferring that "seattle" was the true string
  (println "Observing 'satl'")
  (pprint
    (importance-resampling
      :model spelling-model
      :observation-trace {"with-typos" {"final-string" {:value "satl"}}}
      :n-particles 100))
  (println "Observing 'spatkne'")
  (pprint
    (let [results
          (replicate 100
                     (fn [] (trace-value
                              (importance-resampling
                                :model spelling-model
                                :observation-trace {"with-typos" {"final-string" {:value "spatkne"}}}
                                :n-particles 30)
                              "true-city")))]
      (str "Seattle: " (count (filter (fn [city] (= city "seattle")) results)) ", "
           "Spokane: " (count (filter (fn [city] (= city "spokane")) results))))))

(defn -main []
  (add-errors-demo)
  (spelling-model-demo))