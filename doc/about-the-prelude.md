
Preparing prelude.clj for use

- Start with prelude.vnts (in metaprob repo)
- Convert to Clojure
- Copy from converted/src/ to src/metaprob/src/
- Ad hoc modifications:
    * Remove (:require [metaprob.src.prelude :refer :all])
    * Remove addresses_of because I can't figure out what it's good for.
    * Remove uniform_categorical because I don't understand what it's supposed to do.
    * Remove uniform_continuous because I can't find its definition.
    * Delete `(trace_set (lookup uniform (list "name")) "uniform")`
- Remove anything that's also defined in library.clj or metaprob.clj:
    * `first rest is_pair length last nth range append`
    * `error capture_tag_address env_lookup make_env match_bind`
    * Also remove the declarations for anything that was removed
