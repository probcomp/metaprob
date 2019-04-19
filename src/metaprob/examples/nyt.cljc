(ns metaprob.examples.nyt
  (:require [metaprob.examples.search-by-example :refer [search rowwise-similarity]]
            [metaprob.examples.multimixture-dsl :refer :all]
            [metaprob.examples.cgpm :refer [real-type integer-type make-cgpm]]
            [metaprob.distributions :refer :all]))


(def generate-census-row
    (multi-mixture
     (view
      {"percent_married_children" gaussian
       "percent_black"            gaussian
       "percent_college"          gaussian
       "percap"                   gaussian}

      (clusters
       0.130076 {"percent_married_children" [0.210838 0.038829],
                 "percent_black" [0.095388 0.057366],
                 "percent_college" [0.363796 0.031326],
                 "percap" [34288.191406 2182.854156]}
       0.113952 {"percent_married_children" [0.189332 0.027144],
                 "percent_black" [0.030474 0.016331],
                 "percent_college" [0.227291 0.024654],
                 "percap" [25880.59375 1761.597474]}
       0.081704 {"percent_married_children" [0.186395 0.028675],
                 "percent_black" [0.146349 0.079025],
                 "percent_college" [0.292949 0.018401],
                 "percap" [28796.824219 1501.203346]}
       0.070186 {"percent_married_children" [0.211468 0.03236],
                 "percent_black" [0.028909 0.016286],
                 "percent_college" [0.272871 0.023519],
                 "percap" [29550.820312 1726.099581]}
       0.063276 {"percent_married_children" [0.216061 0.04164],
                 "percent_black" [0.087858 0.057669],
                 "percent_college" [0.438722 0.023252],
                 "percap" [41039.710938 2971.577706]}
       0.056366 {"percent_married_children" [0.22531 0.040431],
                 "percent_black" [0.054432 0.02898],
                 "percent_college" [0.190512 0.026298],
                 "percap" [22245.785156 2110.124032]}
       0.051759 {"percent_married_children" [0.196119 0.037455],
                 "percent_black" [0.056288 0.030392],
                 "percent_college" [0.312046 0.017328],
                 "percap" [31924.5 2239.158358]}
       0.047152 {"percent_married_children" [0.184266 0.026465],
                 "percent_black" [0.199454 0.070668],
                 "percent_college" [0.225004 0.018217],
                 "percap" [25268.396484 1461.17909]}
       0.047152 {"percent_married_children" [0.257421 0.04025],
                 "percent_black" [0.064283 0.044139],
                 "percent_college" [0.535301 0.037194],
                 "percap" [48295.542969 4197.646642]}
       0.044848 {"percent_married_children" [0.151444 0.035401],
                 "percent_black" [0.425126 0.157942],
                 "percent_college" [0.277292 0.029305],
                 "percap" [25863.916016 1815.299233]}
       0.037938 {"percent_married_children" [0.19332 0.044863],
                 "percent_black" [0.114395 0.049581],
                 "percent_college" [0.22875 0.028642],
                 "percap" [25218.994141 1826.126228]}
       0.031028 {"percent_married_children" [0.160177 0.027825],
                 "percent_black" [0.427612 0.130842],
                 "percent_college" [0.194294 0.021655],
                 "percap" [20827.845703 1312.8049]}
       0.028724 {"percent_married_children" [0.245144 0.040058],
                 "percent_black" [0.029762 0.013607],
                 "percent_college" [0.368862 0.026422],
                 "percap" [35795.292969 3072.745678]}
       0.024117 {"percent_married_children" [0.163429 0.037308],
                 "percent_black" [0.381236 0.124864],
                 "percent_college" [0.370864 0.02771],
                 "percap" [33865.132812 3489.038345]}
       0.017207 {"percent_married_children" [0.252309 0.054661],
                 "percent_black" [0.089764 0.072321],
                 "percent_college" [0.119891 0.023823],
                 "percap" [17108.623047 1526.115032]}
       0.154516 {"percent_married_children" [0.203619 0.047071],
                 "percent_black" [0.122003 0.137058],
                 "percent_college" [0.308327 0.105832],
                 "percap" [30963.679441 8647.967362]}))))

  (defn make-identity-output-addr-map
    [output-addrs-types]
    (let [output-addrs (keys output-addrs-types)
          trace-addrs  (map clojure.core/name output-addrs)]
      (clojure.core/zipmap output-addrs trace-addrs)))

  (def census-cgpm
        (let [outputs-addrs-types {;; Variables in the table.
                                   :percent_married_children real-type
                                   :percent_black real-type
                                   :percent_college real-type
                                   :percap real-type

                                   ;; Exposed latent variables.
                                   :cluster-for-percent_married_children integer-type
                                   :cluster-for-percent_black integer-type
                                   :cluster-for-percent_college integer-type
                                   :cluster-for-percap integer-type}
          output-addr-map    (make-identity-output-addr-map outputs-addrs-types)
          inputs-addrs-types {}
          input-addr-map     {}]

      (make-cgpm generate-census-row
                 outputs-addrs-types
                 inputs-addrs-types
                 output-addr-map
                 input-addr-map)))


(comment

  ;; make an example row
  (defn make-ex-row []
    (zipmap [:percent_married_children
             :percent_black
             :percent_college
             :percap]
            (generate-census-row)))

  ;; compare two example rows
  (rowwise-similarity census-cgpm
                      #{:cluster-for-percap}
                      (make-ex-row)
                      (make-ex-row)
                      10)



  ;; make an example table and a single row
  (def example-table
    (repeatedly 10 make-ex-row))
  (def ex (make-ex-row))

  (clojure.pprint/pprint example-table)
  (clojure.pprint/pprint ex)

  ;; search the table by similarity
  (search census-cgpm
          example-table
          (make-ex-row)
          #{:cluster-for-percap} 10)

  )
