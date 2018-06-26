
# Probabilistic inference examples

First load the system into clojure.  A namespace has been prepared for
you that imports everything you will need, so you won't have to worry
about namespace issues; use `in-ns` to enter that namespace.

    (require '[clojure.tools.namespace.repl :refer [refresh]])
    (refresh)
    (in-ns 'metaprob.examples.all)

## Biased coin example

Source code: [metaprob.examples.flip-n-coins](../src/metaprob/examples/flip_n_coins.clj)

First just flip two coins, and look at the resulting output trace:

    (coin-flips-demo-n-flips 2)

Now the same thing but with a coin heavily biased to true (0.99), and
an intervention for one of the flips forcing a false:

    (coin-flips-demo-biased 10)

## Bayes net example

Source code: [metaprob.examples.earthquake](../src/metaprob/examples/earthquake.clj))

Setup:

    (define exact-probabilities 
      (enumerate-executions earthquake-bayesian-network [] (empty-trace) (empty-trace)))
    (define fake-samples
      (fake-samples-for-enumerated-executions exact-probabilities 12240))
    (earthquake-histogram "exact earthquake prior probabilities"
                          fake-samples)

Finish doing the plot at the shell:

    bin/gnuplot-hist results/exact_earthquake_prior_probabilities.samples
    open results/exact_earthquake_prior_probabilities.samples.png

Back to clojure: We can sample from the prior, which should yield
results similar to the exact prior probabilities:

    (define number-of-samples 100)

    (earthquake-histogram "sampled earthquake prior probabilities"
                          (prior-samples number-of-samples))

We can look at exact probabilities in the situation where the alarm went off:

    (define exact-awo-probabilities 
      (enumerate-executions earthquake-bayesian-network [] alarm-went-off (empty-trace)))
    (define fake-awo-samples
      (fake-samples-for-enumerated-executions exact-awo-probabilities 12240))
    (earthquake-histogram "exact earthquake alarm-went-off probabilities"
                          fake-awo-samples)

Estimate the target distribution using two inference methods (should
be about the same as the exact probabilities):

    (earthquake-histogram "samples from earthquake target"
                          (eq-rejection-assay number-of-samples))

    (earthquake-histogram "importance sampling earthquake with 20 particles"
                          (eq-importance-assay 20 number-of-samples))

## 2D gaussian example

Source code: [metaprob.examples.inference-on-gaussian](../src/metaprob/examples/inference_on_gaussian.clj))

For the model, see [metaprob.examples.gaussian](../src/metaprob/examples/gaussian.clj)).

`number-of-runs` is chosen so that rejection sampling runs in a minute
or two.  This means the histograms haven't converged very much.  For
greater accuracy increase `number-of-runs`.

    (define number-of-runs 20)

    (gaussian-histogram "samples from the gaussian demo prior"
                        (gaussian-prior-samples number-of-runs))
    (gaussian-histogram "samples from the gaussian demo target"   
                        (rejection-assay number-of-runs))
    (gaussian-histogram "importance sampling gaussian demo with 20 particles"
                        (importance-assay 20 number-of-runs))
    (gaussian-histogram "samples from gaussian demo lightweight single-site MH with 20 iterations"
                        (MH-assay 20 number-of-runs))
