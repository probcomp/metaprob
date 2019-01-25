(ns metaprob.examples.flip-n-coins-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.builtin :as builtin]
            [metaprob.interpreters :as interp :refer [infer]]
            [metaprob.examples.flip-n-coins :refer :all]))

(defn datum-addr [n]
 (list 2 "datum" "map" n "flip"))

(def number-of-flips 4)

(deftest flip-n-coins-smoke-1
  (testing "testing flip-n-coins"
    (let [[answer trace-with-flips score] (infer :procedure flip-n-coins
                                                 :inputs [number-of-flips])]
      (is (trace? trace-with-flips))

      (let [a1 (builtin/first answer)]
        (is (or (= a1 true)
                (= a1 false))))

      (is (trace-has-value? trace-with-flips (datum-addr (- number-of-flips 1))))
      (is (not (trace-has-value? trace-with-flips (datum-addr number-of-flips))))

      ;; Make sure that intervened-on locations are present
      (is (trace-has-value? trace-with-flips '(0 "tricky" "flip")))
      (is (trace-has-value? trace-with-flips (datum-addr 1)))

      ;; Run subject to interventions
      (let [[answer output score]
            (infer :procedure flip-n-coins
                   :inputs [(+ number-of-flips 8)]
                   :intervention-trace ensure-tricky-and-biased)]
        ;; Check that the interventions actually got done (they're in
        ;; the output trace)
        (doseq [adr (builtin/addresses-of ensure-tricky-and-biased)]

          (is (trace-has-value? output adr))

          (is (= (trace-value output adr)
                 (trace-value ensure-tricky-and-biased adr))))

        (is (trace-has-value?
             output
             '(2 "weight" "then" 0 "uniform")))
        (is (trace-has-value? output (datum-addr 1)))
        (is (trace-has-value? output (datum-addr 2)))
        (is (not (trace-has-value? output (datum-addr (+ number-of-flips 10)))))

        ;; Answer is expected to be 99% heads other than the intervened-on entry.
        (is (> (apply + (map (fn [x] (if x 1 0))
                             answer))
               2))))))

(deftest flip-n-coins-score-1
  (testing "test score returned by flip-n-coins"
    ;; Compute score by re-running with target = previous output
    (let [[output-trace score]
          (loop []
            (let [[_ output-trace _]
                  (infer :procedure flip-n-coins :inputs [number-of-flips]
                         :output-trace? true)
                  [_ _ score]
                  (infer :procedure flip-n-coins :inputs [number-of-flips]
                         :target-trace output-trace
                         :output-trace? false)]
              (let [tricky (trace-value output-trace '(0 "tricky" "flip"))]
                (if tricky
                  (do (print "** tricky, skipping **\n")
                      (recur))
                  [output-trace score]))))
          ;; -2.8779492378976075
          want-score (builtin/log (* 0.9 (builtin/expt 0.5 number-of-flips)))]
      (is (> score (- want-score 0.1)) [score want-score])
      (is (< score (+ want-score 0.1)) [score want-score]))))

(deftest output-trace-1
  (testing "ensure that extraneous values not present in output trace"
    (let [[answer output score]
          (infer :procedure flip-n-coins :inputs [number-of-flips])]
      (is (not (trace-has-value? output '(2))))
      (is (not (trace-has-value? output '(2 "datum"))))
      (is (not (trace-has-value? output '(2 "datum" "map"))))
      (is (not (trace-has-value? output '(2 "datum" "map" 1)))))))
