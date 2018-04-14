
# Probabilistic inference examples

A namespace has been prepared for you that imports everything you will
need, so you won't have to worry about namespace issues.

    (in-ns 'metaprob.examples.all)

## Biased coin example

Source: [metaprob.examples.flip-n-coins](../src/metaprob/examples/flip_n_coins.clj)

    (coin-flips-demo-n-flips 2)
    (coin-flips-demo-biased 10)

## Bayes net example

Source: [metaprob.examples.earthquake](../src/metaprob/examples/earthquake.clj))

    (define exact-probabilities 
      (enumerate-executions earthquake-bayesian-network [] (empty-trace) (empty-trace)))
    (define fake-samples
      (fake-samples-for-enumerated-executions exact-probabilities 12240))
    (earthquake-histogram "exact earthquake prior probabilities"
                          fake-samples)

Finish doing the plot at the shell:

    gnuplot-hist results/exact_earthquake_prior_probabilities.samples
    open results/exact_earthquake_prior_probabilities.samples.png

Back to clojure:

    ;; Exact alarm-went-off probabilities
    (define exact-probabilities 
      (enumerate-executions earthquake-bayesian-network [] alarm_went_off (empty-trace)))
    (define fake-samples
      (fake-samples-for-enumerated-executions exact-probabilities 12240))
    (earthquake-histogram "exact earthquake alarm-went-off probabilities"
                          fake-samples)

    (define number-of-samples 100)

    ;; Sampling from the prior
    (earthquake-histogram "sampled earthquake prior probabilities"
                          (prior-samples number-of-samples))

    ;; Rejection sampling
    (earthquake-histogram "samples from the target"
                          (eq-rejection-assay number-of-samples))

    ;; Importance sampling
    (earthquake-histogram "samples from importance sampling with 20 particles"
                          (eq-importance-assay 20 number-of-samples))


## 2D gaussian example

Source: [metaprob.examples.inference-on-gaussian](../src/metaprob/examples/inference_on_gaussian.clj))

    (define number-of-runs 100)

    (gaussian-histogram "samples from the prior"
                        (gaussian-prior-samples number-of-runs))
    (gaussian-histogram "samples from the target"   
                        (rejection-assay number-of-runs))
    (gaussian-histogram "samples from importance sampling with 20 particles"
                        (importance-assay 20 number-of-runs))
    (gaussian-histogram "samples from lightweight single-site MH with 20 iterations"
                        (MH-assay 20 number-of-runs))
