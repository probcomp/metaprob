(ns dontknow.biased-coin
  (:refer-clojure :only [])
  (:require [dontknow.builtin :refer :all]))

(define flip-coins
  (program [n]
    (let [tricky (flip 0.1)
          weight (if tricky
                   (uniform 0 1)
                   0.5)]
      (map (program [i] (flip weight))
           (range n)))))
