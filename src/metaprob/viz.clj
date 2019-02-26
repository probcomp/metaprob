(ns metaprob.viz-server
  (:require [org.httpkit.server :as httpkit]
            [compojure.core :refer [defroutes GET POST DELETE ANY context]]
            [compojure.route :refer [files not-found]]
            [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]
            [ring.util.response :refer [file-response]]
            [cheshire.core :as json]))

(defonce server (atom nil))
(defonce visualizations (atom {}))

(defn handle-connect
  [{:keys [clientId vizId]} channel]
  (do
    (println "looks like a connection from" clientId "to" vizId "on" channel)
    (swap! visualizations assoc-in [vizId :clients clientId] channel)
    (println @visualizations)
    (httpkit/send! channel (json/generate-string
                            {:action "initialize"
                             :traces (get-in @visualizations [vizId :traces])
                             :info   (get-in @visualizations [vizId :info])}))))

(defn handle-disconnect [{:keys [clientId vizId]}]
  (do (println "Disconnect from " clientId)
      (swap! visualizations update-in [vizId :clients] dissoc clientId)
      (println @visualizations)))

(defn ws-handler [request]
  (httpkit/with-channel request channel
    (httpkit/on-receive channel
                        (fn [data]
                          (let [msg (json/parse-string data true)]
                            (do (println "Got over ws:" msg)
                                (case (:action msg)
                                  "connect" (handle-connect msg channel)
                                  "disconnect" (handle-disconnect msg))))))))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn viz-disk-path [v]
  (get-in @visualizations [v :path]))

(defn wrap-viz
  "Requests for visualizations arrive looking like:

       http://localhost/<visualization-id>/

  and

      http://localhost/<visualization-id>/path/to/main.js

  Attempt to map `visualization-id` to a registered visualization. If
  we succeed, return static HTML/js/CSS of the 'renderer' of the
  registered visualization. If we fail to map the ID, call the wrapped
  handler.
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
  "This is a websocket request? Bounce it to the websocket
  handler. Otherwise, carry on."
  [handler]
  (fn [req]
    (if (:websocket? req)
      (do (println "Looks like a websocket connection")
          (ws-handler req))
      (handler req))))

(defroutes routes
  (GET "/" [] "<html><body>Metaprob Viz Server. Hi!</body></html>")
  (files "/public")
  (not-found "<p>Page not found.</p>"))

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

(comment
  (reset! server (httpkit/run-server
                  (-> #'routes
                      wrap-ws
                      wrap-viz)
                  {:port 8081}))
  (stop-server)
  (reset! visualizations {})
  (swap! visualizations assoc "1234" {:path "public/vue/dist/"})
  (def vid (add-viz! "public/vue/dist/" [[-1.0 -0.5 0 0.5 1]
                                         [-1.2 -0.39 0.02 0.56 0.98]]))

  (println vid)

  )
