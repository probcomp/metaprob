(ns metaprob.main
  (:gen-class)
  (:require [metaprob.to-clojure]))

(defn -main [inpath outpath nsname]
  (println (list 'in- inpath 'out- outpath))

  (if inpath
    (if outpath
      (metaprob.to-clojure/convert inpath outpath (symbol nsname))
      (throw (Exception. "no outpath")))
    (throw (Exception. "no inpath"))))
