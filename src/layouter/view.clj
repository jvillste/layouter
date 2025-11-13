(ns layouter.view
  (:require
   [clojure.core.async :as async]
   [fungl.application :as application]))

(defonce event-channel-atom (atom nil))

(defn refresh-view! []
  (when @event-channel-atom
    (async/>!! @event-channel-atom
               #_{:type :redraw}
               {:type :foo})))

(comment
  (reset! event-channel-atom nil)
  )

(defn start-view [view & [{:keys [join?]}]]
  (reset! event-channel-atom
          (application/start-application view
                                         :on-exit #(reset! event-channel-atom nil)
                                         :join? join?)))
