(ns layouter.gui
  (:require
   [clojure.test :refer [deftest is]]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.visuals :as visuals]
   [fungl.component.text-area :as text-area]
   [fungl.layouts :as layouts]
   [layouter.view :as view]))

(def theme (let [text-color [200 200 200 255]
                 background-color [0 0 0 255]]
             {:background-color background-color
              :text-color text-color}))

(defn box [content & [{:keys [padding fill-color draw-color line-width corner-arc-radius]
                       :or {fill-color (:background-color theme)
                            draw-color (:background-color theme)
                            line-width 2
                            padding 2
                            corner-arc-radius 5}}]]
  (layouts/box padding
               (visuals/rectangle-2 :fill-color fill-color
                                    :draw-color draw-color
                                    :line-width line-width
                                    :corner-arc-radius corner-arc-radius)
               content))

(def font-size 50)
(def font (font/create-by-name "CourierNewPSMT" font-size))

(defn text [string & [{:keys [font color] :or {font font
                                               color (:text-color theme)}}]]
  (text-area/text (str string)
                  color
                  font))

(defn black-background [contents]
  (layouts/superimpose (visuals/rectangle-2 {:fill-color [0 0 0 255]})
                       contents))

(defn multiply-color [multiplier color]
  (vec (concat (map (partial * multiplier)
                    (take 3 color))
               [(last color)])))

(deftest test-multiply-color
  (is (= [2 2 2 1]
         (multiply-color 2 [1 1 1 1]))))


(defn highlight [content & [{:keys [fill-color] :or {fill-color [240 240 255 255]}}]]
  (layouts/box 5
               (visuals/rectangle-2 :fill-color fill-color
                                    :draw-color nil
                                    :line-width 0
                                    :corner-arc-radius 20)
               content))

(defn maby-highlight [highlight? child]
  (highlight child
             {:fill-color (if highlight?
                            [0 0.5 0 1.0]
                            [0 0 0 0])}))

(view/hard-refresh-view!)
