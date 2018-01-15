(ns dontknow.from-clojure-test
  (:require [clojure.test :refer :all]
            [dontknow.to-clojure :refer :all]
            [dontknow.from-clojure :refer :all]))

(deftest invert-1
  (testing "Smoke test for from/to-clojure"
    (let [sample '(f x y)]
      (is (= sample (to-clojure (from-clojure sample)))))))

(deftest invert-2
  (testing "Smoke test for from/to-clojure"
    (let [sample '(program [x] 7 x)]
      (is (= sample (to-clojure (from-clojure sample)))))))
