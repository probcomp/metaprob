(ns metaprob.tutorial.jupyter
  (:require
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojupyter.misc.display :as display]
   [metaprob.builtin :as builtin]))

(defn enable-inline-viz []
  (display/hiccup-html
    [:div [:em "Inline visualization functions have been enabled."]
     [:script (slurp (io/resource "plotly-latest.min.js"))]
     [:script (slurp (io/resource "plot-trace.js"))]]))

(defn trace-as-json
  [tr]
  (let [base (if (builtin/trace-has-value? tr)
               (let [v (builtin/trace-value tr)]
                 {:value (if (float? v)
                           (format "%f" v)
                           (pr-str v))})
               {})
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
;
;(defn histogram
;  ([data] (histogram data 20))
;  ([data bins]
;    (histogram data (apply min data) (apply max data) bins))
;  ([data min max bins]
;    (let [id (str "svg" (java.util.UUID/randomUUID))
;          code (format "drawHistogram(\"%s\", %s, %d, %d, %d);" id, (json/write-str data), (int (Math/floor min)), (int (Math/ceil max)), bins)]
;      (display/hiccup-html
;        [:div {:style (format "height: 500; width: 960")} [:svg {:id id :width 960 :height 500}]
;         [:script code]]))))


(defn plotly-chart
  [data layout options]
  (let [id (str "plotly" (java.util.UUID/randomUUID))
         code (format "Plotly.newPlot(%s, %s, %s, %s)" (json/write-str id) (json/write-str data) (json/write-str layout) (json/write-str options))]
     (display/hiccup-html [:div [:div {:id id}] [:script code]])))


(defn plotly-chart-2
  [options]
  (let [id (str "plotly" (java.util.UUID/randomUUID))
        code (format "Plotly.plot(%s, %s)" (json/write-str id) (json/write-str options))]
    (display/hiccup-html [:div [:div {:id id}] [:script code]])))

(defn plotly-chart-animated
  [initial-data datas layout options]
  (let [id (str "plotly" (java.util.UUID/randomUUID))
        code (format "Plotly.newPlot(%s, %s, %s, %s); plotlyAnimate(%s, %s)"
                     (json/write-str id) (json/write-str initial-data)
                     (json/write-str layout) (json/write-str options)
                     (json/write-str id) (json/write-str datas))]
    (display/hiccup-html [:div [:div {:id id}] [:script code]])))



(defn bar-chart
  [title labels data]
  (plotly-chart [{:x labels :y data :type "bar" :hoverinfo "none"}] {:title title} {:displayModeBar false}))

(defn histogram
  ([title data [min-x max-x]]
  (plotly-chart [{:x data :type "histogram" :xbins {:start min-x :end max-x}}]
                {:title title :xaxis {:range [min-x max-x]}}
                {:displayModeBar false}))
  ([title data [min-x max-x] bin-size]
   (plotly-chart [{:x data :type "histogram" :xbins {:start min-x :end (+ max-x bin-size) :size bin-size} :autobinx false}]
                 {:title title :xaxis {:range [min-x (+ max-x bin-size)]}}
                 {:displayModeBar false}))
  ([title data] (plotly-chart [{:x data :type "histogram"}]
                              {:title title}
                              {:displayModeBar false})))

(defn overlaid-histograms
  [title name1 dist1 name2 dist2]
  (plotly-chart [{:x dist1 :name name1 :type "histogram" :opacity 0.5 :marker {:color "green"}}
                 {:x dist2 :name name2 :type "histogram" :opacity 0.5 :marker {:color "red"}}]
                {:barmode "overlay" :title title}
                {:displayModeBar false}))

(defn lin-range
  [low high n-intervals]
  (map (fn [i] (+ low (* i (/ (- high low) (- n-intervals 1)))))
       (range n-intervals)))

(defn curve-trace
  [density [x-min x-max]]
  (let
    [x-coords (lin-range x-min x-max 100)
     curve-data (vec (map density x-coords))]
    {:x x-coords :y curve-data :type "line" :name "density" :yaxis "y2"}))

(defn histogram-with-curve
  [title data density x-range]
  (plotly-chart [(curve-trace density x-range),
                 {:x data :name "samples" :type "histogram"}]
                {:title title,
                 :xaxis {:title "x"}
                 :yaxis {:title "count"},
                 :yaxis2 {:overlaying "y" :rangemode "tozero" :zeroline false :showgrid false :anchor "y" :title "density" :side "right"}}
                {:displayModeBar false}))

; Produce a Plotly trace (not a Metaprob trace!)
; specifying options for a contour plot of the given density.
(defn contour-trace
  [density [[x-min x-max] [y-min y-max]]]
  (let [x-coords (lin-range x-min x-max 40)
        y-coords (lin-range y-min y-max 40)
        contour-data
        (vec
          (map (fn [y] (clojure.core/vec (map (fn [x] (density x y)) x-coords)))
               y-coords))]
    {:x x-coords :y y-coords :z contour-data :type "contour", :name "density"}))

; Produce a Plotly trace (not a Metaprob trace)
; with options for a scatter plot with the given data
; in the form ([x1 y1], [x2 y2], ...)
(defn scatter-trace
  [data-points marker-symbol color]
  {:x (map first data-points),
   :y (map second data-points),
   :mode "markers", :type "scatter", :hoverinfo "none",
   :id "samples",
   :marker {:color color :symbol marker-symbol}})
(defn little-scatter-trace [data-points]
  {:x (map first data-points)
   :id "samples"
   :y (map second data-points)})

(defn default-scatter-plot
  [title data]
  (plotly-chart [(scatter-trace data "cross" "black")]
                {:xaxis {:title "x"}
                 :yaxis {:title "y"}
                 :title title}
                {:displayModeBar false}))

(defn custom-scatter-plot
  [title data marker-symbol color bgcolor [x-range y-range]]
  (plotly-chart [(scatter-trace data marker-symbol color)]
                {:xaxis {:range x-range :title "x"}
                 :yaxis {:range y-range :title "y"}
                 :plot_bgcolor bgcolor
                 :title title
                 :width 500
                 :height 500}
                {:displayModeBar false}))

(defn scatter-with-contours-animated
  [title data density [x-range y-range]]
  (let
    [
     init-data [(contour-trace density [x-range y-range]) (scatter-trace (first data) "cross" "black")]
     slider-steps
     (vec (map (fn [i] {:method "animate" :label (str i) :args [[i] {:mode "immediate" :transition {:duration 50}}]}) (range (count data))))
     layout {:xaxis {:range x-range :title "x" :zeroline false}
             :yaxis {:range y-range :title "y" :zeroline false}
             :width 500
             :height 500
             :sliders [{:currentvalue {:visible true :prefix "Iter:" :xanchor "left"} :steps slider-steps}]
             :title title}
     config {:displayModeBar false}
     frames (vec (map-indexed (fn [i d] {:name (str i) :data [{} (little-scatter-trace d)]}) data))
     options {:data init-data :layout layout :config config :frames frames}
     ]
    (plotly-chart-2 options)))

(defn mh-animation
  [title data [x-range y-range]]
  (let
     [
      init-data [(scatter-trace [(first data)] "cross" "black")]
      frames (vec (reverse (map-indexed (fn [i x] {:name (str i) :data [x]}) (reduce
                    (fn [l next]
                      (if (and (= (((first l) :y) 0) (next 1))
                               (= (((first l) :x) 0) (next 0)))
                        (cons
                          {:x [(next 0)] :y [(next 1)] :marker {:color "red"}}
                          (cons {:x [(next 0)] :y [(next 1)] :marker {:color "white"}}
                            l))
                        (cons {:x [(next 0)] :y [(next 1)] :marker {:color "black"}}
                        l)))
                    (list {:x [((first data) 0)] :y [((first data) 1)] :marker {:color "black"}})
                    (rest data)))))
      layout {:xaxis {:range x-range :title "x"}
              :yaxis {:range y-range :title "y"}
              :width 500 :height 500
              :title title
              :updatemenus
                     [{ :type "buttons" :xanchor "left" :yanchor "top" :pad {:t 50}
                       :x 0 :y 0
                       :buttons [{:method "animate" :args [nil {:mode "immediate" :transition {:duration 600} :frame {:duration 500 :redraw false}}]
                                  :label "Play"}] }]}
      config {:displayModeBar false}
      ]
     (plotly-chart-2 {:data init-data :frames frames :layout layout :config config})))

(defn scatter-with-contours
  [title data density [x-range y-range]]
  ; generate contour data
  (plotly-chart [(contour-trace density [x-range y-range]),
                 (scatter-trace data "cross" "white")]
                {:xaxis {:range x-range :title "x"}
                 :yaxis {:range y-range :title "y"}
                 :width 500
                 :height 500
                 :title title}
                {:displayModeBar false}))