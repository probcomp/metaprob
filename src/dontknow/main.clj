(ns dontknow.main
  (:gen-class)
  (:require [dontknow.to-clojure]))

(defn -main [inpath outpath nsname]
  (print (list 'in- inpath 'out- outpath))
  (newline)
  (flush)

  (if inpath
    (if outpath
      (dontknow.to-clojure/convert inpath outpath (symbol nsname))
      (throw (Exception. "no outpath")))
    (throw (Exception. "no inpath")))
)
