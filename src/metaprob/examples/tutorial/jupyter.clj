(ns metaprob.examples.tutorial.jupyter
  (:require
    [clojupyter.misc.display :as display]
    [clojure.data.json :as json]
    [metaprob.builtin :as builtin]))

(defn enable-trace-plots []
  (display/hiccup-html
    [:div [:em "Trace visualization has been enabled."] [:script (slurp "src/metaprob/examples/tutorial/plot-trace.js")]]))

(defn trace-as-json
  [tr]
  (let [base (if (builtin/trace-has? tr) {:value (pr-str (builtin/trace-get tr))} {})
        children (for [key (builtin/trace-keys tr)]
                   (into (trace-as-json (builtin/trace-subtrace tr key)) [[:name (pr-str key)]]))]
    (into base [[:children (vec children)]])))

(defn plot-trace
  ([trace-json] (plot-trace trace-json 600 600))
  ([trace-json s] (plot-trace trace-json s s))
  ([trace-json w h]
   (let [id (str "svg" (java.util.UUID/randomUUID))
         code (format  "drawTrace(\"%s\", %s, %d, %d);" id, (json/write-str (trace-as-json trace-json)), h, w)]
     (display/hiccup-html
       [:div {:style (format "height: %d; width: %d" (+ h 50) (+ w 100))} [:svg {:id id :width (+ w 100) :height h}]
        [:script code]]))))

(defn histogram
  ([data] (histogram data 20))
  ([data bins]
    (histogram data (apply min data) (apply max data) bins))
  ([data min max bins]
    (let [id (str "svg" (java.util.UUID/randomUUID))
          code (format "drawHistogram(\"%s\", %s, %d, %d, %d);" id, (json/write-str data), (int (Math/floor min)), (int (Math/ceil max)), bins)]
      (display/hiccup-html
        [:div {:style (format "height: 500; width: 960")} [:svg {:id id :width 960 :height 500}]
         [:script code]]))))