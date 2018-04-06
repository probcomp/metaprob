
(ns metaprob.oopsla.flip-n-coins-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :as trace]
            [metaprob.builtin :as builtin]
            [metaprob.oopsla.interpreters :refer :all]
            [metaprob.oopsla.flip-n-coins :refer :all]))

(deftest flip-n-coins-smoke-1
  (testing "testing flip-n-coins"
    (let [n 2
          trace-with-flips (builtin/empty-trace)]
      (let [[answer score]
            (infer :procedure flip-n-coins :inputs (builtin/tuple n)
                   :output-trace trace-with-flips)]
        (is (builtin/list? answer))
        (let [a1 (builtin/nth answer 0)]
          (is (or (= a1 true) (= a1 false))))
        (is (builtin/trace-has? trace-with-flips (builtin/addr "datum" (- n 1) "flip")))
        (is (not (builtin/trace-has? trace-with-flips (builtin/addr "datum" n "flip"))))

        ;; Compute score by re-running with target = previous output
        (let [[answer score]
              (infer :procedure flip-n-coins :inputs (builtin/tuple n)
                     :target-trace trace-with-flips)]
          (is (builtin/list? answer))
          (is (< score 0))     ; e.g. -1.49

          ;; Make sure that intervened-on locations are present
          (is (builtin/trace-has? trace-with-flips (builtin/addr 1 "tricky" "flip")))
          (is (builtin/trace-has? trace-with-flips (builtin/addr "datum" 1 "flip")))

          ;; Run subject to interventions
          (let [output (builtin/empty-trace)
                [answer score]
                (infer :procedure flip-n-coins :inputs (builtin/tuple (+ n 8))
                       :intervention-trace ensure-tricky-and-biased
                       :output-trace output)]
            ;; Check that the interventions actually got done
            (doseq [adr (builtin/addresses-of ensure-tricky-and-biased)]
              (is (builtin/trace-has? output adr))
              (if (builtin/trace-has? output adr)
                (is (= (builtin/trace-get output adr)
                       (builtin/trace-get ensure-tricky-and-biased adr)))))

            ;; (is (builtin/trace-has? output (builtin/addr 2 "weight" "then" 0 "uniform")))
            (is (builtin/trace-has? output (builtin/addr "datum" 1 "flip")))
            (is (builtin/trace-has? output (builtin/addr "datum" 2 "flip")))
            (is (not (builtin/trace-has? output (builtin/addr "datum" 10 "flip"))))

            ;; Answer is expected to be 99% heads other than the intervened-on entry.
            (is (> (apply + (map (fn [x] (if x 1 0)) (trace/metaprob-sequence-to-seq answer)))
                   2))

            ))))))
