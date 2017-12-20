(ns dontknow.biased-coin
  (:require [dontknow.trie :refer :all])
  (:require [dontknow.builtin :refer :all]))

(defn flip-coins [n]
  (let [tricky (flip 0.1)
        weight (if tricky
                 (uniform 0 1)
                 0.5)]
    (map (fn [i] (flip weight))
         (range n))))
