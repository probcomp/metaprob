(ns metaprob.examples.flip-n-coins-test
  (:refer-clojure :exclude [map replicate apply])
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.examples.flip-n-coins :refer :all]))

(defn datum-addr [n] n)

(def number-of-flips 4)

(deftest flip-n-coins-smoke-1
  (testing "testing flip-n-coins"
    (let [[answer trace-with-flips score] (infer-and-score :procedure flip-n-coins
                                                           :inputs [number-of-flips])]
      (is (trace? trace-with-flips))

      (let [a1 (first answer)]
        (is (or (= a1 true)
                (= a1 false))))

      (is (trace-has-value? trace-with-flips (- number-of-flips 1)))
      (is (not (trace-has-value? trace-with-flips number-of-flips)))

      ;; Make sure that observed locations are present
      (is (trace-has-value? trace-with-flips "tricky"))
      (is (trace-has-value? trace-with-flips 1))

      ;; Run subject to observations
      (let [[answer output score]
            (infer-and-score :procedure flip-n-coins
                             :inputs [(+ number-of-flips 8)]
                             :observation-trace ensure-tricky-and-biased)]

        ;; Check that the interventions actually got done (they're in
        ;; the output trace)
        (doseq [adr (addresses-of ensure-tricky-and-biased)]
          (is (trace-has-value? output adr))
          (is (= (trace-value output adr)
                 (trace-value ensure-tricky-and-biased adr))))

        (is (trace-has-value? output "p"))
        (is (trace-has-value? output 1))
        (is (trace-has-value? output 2))
        (is (not (trace-has-value? output (+ number-of-flips 10))))

        ;; Answer is expected to be 99% heads other than the intervened-on entry.
        (is (> (apply + (map (fn [x] (if x 1 0)) answer)) (+ number-of-flips 5)))))))

(deftest flip-n-coins-score-1
  (testing "test score returned by flip-n-coins"
    ;; Compute score by re-running with target = previous output
    (let [[output-trace score]
          (loop []
            (let [[_ output-trace _]
                  (infer-and-score :procedure flip-n-coins :inputs [number-of-flips])
                  [_ _ score]
                  (infer-and-score :procedure flip-n-coins :inputs [number-of-flips] :observation-trace output-trace)]
              (let [tricky (trace-value output-trace "tricky")]
                (if tricky
                  (do (print "** tricky, skipping **\n")
                      (recur))
                  [output-trace score]))))
          ;; -2.8779492378976075
          want-score (log (* 0.9 (expt 0.5 number-of-flips)))]
      (is (> score (- want-score 0.1)) [score want-score])
      (is (< score (+ want-score 0.1)) [score want-score]))))
