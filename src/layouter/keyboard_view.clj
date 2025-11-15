(ns layouter.keyboard-view
  (:require
   [fungl.layouts :as layouts]
   [fungl.layouts :as layouts]
   [layouter.gui :as gui]
   [layouter.keyboard :as keyboard]))

(def one-hand-finger-colors [[105 49 70 255]
                             [70 49 105 255]
                             [105 105 49 255]
                             [70 105 49 255]])

(def both-hands-finger-colors
  (into [] (concat one-hand-finger-colors
                   (reverse one-hand-finger-colors))))

(def key-colors-for-fingers (into {}
                                  (for [keyboard-key keyboard/keyboard-keys]
                                    [(:cocoa-key-code keyboard-key)
                                     (get both-hands-finger-colors
                                          (:finger keyboard-key))])))

(def key-size 30)

(defn row-view [cocoa-key-codes cocoa-key-code-to-character key-color on-event]
  (layouts/horizontally-2 {:margin 1}
                          (for [cocoa-key-code cocoa-key-codes]
                            (let [character (cocoa-key-code-to-character cocoa-key-code)]
                              {:node (gui/box (layouts/with-minimum-size key-size key-size (gui/text character))
                                              {:fill-color (or (key-color cocoa-key-code)
                                                               [0 0 0 0])
                                               :padding 5})
                               :mouse-event-handler (fn [node event]
                                                      (when (= :nodes-under-mouse-changed (:type event))
                                                        (if (contains? (set (map :id (:nodes-under-mouse event)))
                                                                       (:id node))
                                                          (when on-event
                                                            (on-event {:type :mouse-entered-character
                                                                       :character character}))
                                                          (when on-event
                                                            (on-event {:type :mouse-left-character
                                                                       :character character}))))
                                                      (when (and (= :mouse-pressed (:type event))
                                                                 on-event)
                                                        (on-event {:type :mouse-pressed
                                                                   :character character}))
                                                      event)}))))

(defn keyboard-view [cocoa-key-code-to-character key-color & [{:keys [on-key-event]}]]
  (layouts/vertically-2 {:margin 10}
                        (layouts/vertically-2 {:margin 1}
                                              (layouts/with-margins 0 0 0 (* 0.3 key-size)
                                                (layouts/horizontally-2 {:margin 10}
                                                                        (row-view [12 13 14 15 17]
                                                                                  cocoa-key-code-to-character
                                                                                  key-color
                                                                                  on-key-event)
                                                                        (row-view [16 32 34 31 35 33]
                                                                                  cocoa-key-code-to-character
                                                                                  key-color
                                                                                  on-key-event)))
                                              (layouts/with-margins 0 0 0 (* 0.7 key-size)
                                                (layouts/horizontally-2 {:margin 10}
                                                                        (row-view [0 1 2 3 5]
                                                                                  cocoa-key-code-to-character
                                                                                  key-color
                                                                                  on-key-event)
                                                                        (row-view [4 38 40 37 41 39]
                                                                                  cocoa-key-code-to-character
                                                                                  key-color
                                                                                  on-key-event)))
                                              (layouts/with-margins 0 0 0 0
                                                (layouts/horizontally-2 {:margin 10}
                                                                        (row-view [50 6 7 8 9 11]
                                                                                  cocoa-key-code-to-character
                                                                                  key-color
                                                                                  on-key-event)
                                                                        (row-view [45 46 43 47 44]
                                                                                  cocoa-key-code-to-character
                                                                                  key-color
                                                                                  on-key-event))))))
