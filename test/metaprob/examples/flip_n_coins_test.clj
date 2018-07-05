
(ns metaprob.examples.flip-n-coins-test
  (:require [clojure.test :refer :all]
            [metaprob.builtin :as builtin]
            [metaprob.interpreters :as interp :refer [infer]]
            [metaprob.examples.flip-n-coins :refer :all]))

(defn datum-addr [n]
  (builtin/addr 2 "datum" "map" n "flip"))

(deftest flip-n-coins-smoke-1
  (testing "testing flip-n-coins"
    (let [n 4
          trace-with-flips (builtin/empty-trace)]
      (builtin/print "trace-with-flips = ") (builtin/pprint trace-with-flips)
      (let [[answer score]
            (infer :procedure flip-n-coins :inputs (builtin/tuple n)
                   :output-trace trace-with-flips)]
        (builtin/print "trace-with-flips = ") (builtin/pprint trace-with-flips)
        (is (builtin/list? answer))
        (let [a1 (builtin/nth answer 0)]
          (is (or (= a1 true) (= a1 false))))
        (is (builtin/trace-has? trace-with-flips (datum-addr (- n 1))))
        (is (not (builtin/trace-has? trace-with-flips (datum-addr n))))

        ;; Compute score by re-running with target = previous output
        (let [[answer score]
              (infer :procedure flip-n-coins :inputs (builtin/tuple n)
                     :target-trace trace-with-flips)]
          (is (builtin/list? answer))
          (is (< score 0))     ; e.g. -1.49

          ;; Make sure that intervened-on locations are present
          (is (builtin/trace-has? trace-with-flips (builtin/addr 0 "tricky" "flip")))
          (is (builtin/trace-has? trace-with-flips (datum-addr 1)))

          ;; Run subject to interventions
          (let [output (builtin/empty-trace)
                [answer score]
                (infer :procedure flip-n-coins :inputs (builtin/tuple (+ n 8))
                       :intervention-trace ensure-tricky-and-biased
                       :output-trace output)]
            ;; Check that the interventions actually got done (they're in the output trace)
            (doseq [adr (builtin/addresses-of ensure-tricky-and-biased)]
              (is (builtin/trace-has? output adr))
              (if (builtin/trace-has? output adr)
                (is (= (builtin/trace-get output adr)
                       (builtin/trace-get ensure-tricky-and-biased adr)))))

            ;; (is (builtin/trace-has? output (builtin/addr 2 "weight" "then" 0 "uniform")))
            (is (builtin/trace-has? output (datum-addr 1)))
            (is (builtin/trace-has? output (datum-addr 2)))
            (is (not (builtin/trace-has? output (datum-addr (+ n 10)))))

            ;; Answer is expected to be 99% heads other than the intervened-on entry.
            (is (> (apply + (map (fn [x] (if x 1 0)) (builtin/sequence-to-seq answer)))
                   2))

            ))))))
