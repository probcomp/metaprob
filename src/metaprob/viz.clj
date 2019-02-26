(ns metaprob.viz-server
  (:require [org.httpkit.server :as httpkit]
            [compojure.core :refer [defroutes GET ]]
            [compojure.route :refer [files not-found]]
            [clojure.string :as str]
            [ring.util.response :refer [file-response]]
            [cheshire.core :as json]))

(defonce server (atom nil))
(defonce visualizations (atom {}))

(defn uuid [] (str (java.util.UUID/randomUUID)))

;;;;;;;;;;
;; websocket handling
;;;;;;;;;;
(defn handle-connect
  [{:keys [clientId vizId]} channel]
  (do
    (swap! visualizations assoc-in [vizId :clients clientId] channel)
    (httpkit/send! channel (json/generate-string
                            {:action "initialize"
                             :traces (get-in @visualizations [vizId :traces])
                             :info   (get-in @visualizations [vizId :info])}))))

(defn handle-disconnect [{:keys [clientId vizId]}]
  (swap! visualizations update-in [vizId :clients] dissoc clientId)
  (println @visualizations))

(defn ws-handler
  "Ring handler for managing the web socket. Accept 'messages', dispatch
  to functions based on :action"
  [request]
  (httpkit/with-channel request channel
    (httpkit/on-receive channel
                        (fn [data]
                          (let [msg (json/parse-string data true)]
                            (do (println "Got over ws:" msg)
                                (case (:action msg)
                                  "connect" (handle-connect msg channel)
                                  "disconnect" (handle-disconnect msg))))))))


;;;;;;;;;;
;; ring middleware
;;;;;;;;;;

(defn viz-disk-path
  "Given a visualization ID, return the path to its renderer"
  [v]
  (get-in @visualizations [v :path]))

(defn wrap-viz
  "Ring middleware for handling requests involving visualization IDs,
  which return static content based on the path of the
  visualzation. Requests for visualizations arrive looking like:

       http://localhost/<visualization-id>/

  and

      http://localhost/<visualization-id>/path/to/main.js

  Attempt to map `visualization-id` to a registered visualization. If
  we succeed, return the static HTML/js/CSS content being requested,
  rooted at the visualization's :path. If we fail to map the ID, call
  the wrapped handler.
  "
  [handler]
  (fn [{:keys [uri] :as req}]
    (let [path-elems   (rest (str/split uri #"/+"))
          viz-id       (first path-elems)
          viz-resource (if-let [res (seq (rest path-elems))]
                         (str/join "/" res)
                         "index.html")
          viz-path     (viz-disk-path viz-id)]

      (if (nil? viz-path)
        (handler req)
        (file-response viz-resource {:root viz-path})))))

(defn wrap-ws
  "Ring middleware to detect if the request is a websocket upgrade
  request, in which case we call our websocket handler. If not, we
  pass the request through unchanged."
  [handler]
  (fn [req]
    (if (:websocket? req)
      (ws-handler req)
      (handler req))))

;; The bottom of our handler stack: serve static files, a placeholder
;; index page, 404's.
(defroutes routes
  (GET "/" [] "<html><body>Metaprob Viz Server. Hi!</body></html>")
  (files "/public")
  (not-found "<p>Page not found.</p>"))

;;;;;;;;;
;; API- functions to register visualizations, to add and remove traces
;;;;;;;;;

(defn add-viz!
  [path info]
  (let [id (uuid)]
    (swap! visualizations assoc id
           {:path path
            :clients {} ;; uuid -> ws
            :info info
            :traces {}  ;; uuid -> trace
            :id id
            :html ""})
    id))

(defn put-trace!
  [viz-id trace]
  (let [trace-id (uuid)]
    (swap! visualizations assoc-in [viz-id :traces trace-id] trace)
    (doall (map (fn [channel]
                  (httpkit/send! channel (json/generate-string
                                          {:action "putTrace"
                                           :tId    trace-id
                                           :t      trace})))
                (vals (get-in @visualizations [viz-id :clients]))))
    trace-id))

(defn delete-trace!
  [viz-id trace-id]
  (do
    (swap! visualizations update-in [viz-id :traces] dissoc trace-id)
    (doall (map (fn [channel]
                  (httpkit/send! channel (json/generate-string
                                          {:action "removeTrace"
                                           :tId    trace-id})))
                (vals (get-in @visualizations [viz-id :clients]))))))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(comment
  ;; create a server and start it listening
  (reset! server (httpkit/run-server
                  (-> #'routes
                      wrap-ws
                      wrap-viz)
                  {:port 8081}))

  ;; stop the running server
  (stop-server)

  ;; clear any visualizations (does not update the clients)
  (reset! visualizations {})


  ;; add an example visualization
  (def vid (add-viz! "public/vue/dist/" [[-2.0 -1.0    0 1.0 2.0]
                                         [-2.0  -1.0   0 1.0 2.0]]))

  (println vid)
  ;; try visiting `http://localhost:8081/<vid>/`

  ;; define an example trace
  (def t {:slope 1.4
          :intercept 0
          :inlier_std 0.2
          :outlier_std 1.2
          :outliers [false false true false true]})

  ;; add that trace. this should update the connected client
  (def tid (put-trace! vid t))

  ;; remove the trace
  (delete-trace! vid tid)
  )
