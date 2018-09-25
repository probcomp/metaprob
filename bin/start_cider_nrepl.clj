(require '[cider-nrepl.main])

(cider-nrepl.main/start-nrepl
 {:bind "0.0.0.0"
  :port 9009
  :middleware ["refactor-nrepl.middleware/wrap-refactor"
               "cider.nrepl/cider-middleware"]})
