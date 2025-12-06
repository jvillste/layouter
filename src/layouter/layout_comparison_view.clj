(ns layouter.layout-comparison-view
  (:require
   [clojure.set :as set]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.path :as path]
   [flow-gl.gui.visuals :as visuals]
   [flow-gl.gui.visuals :as visuals]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [fungl.layouts :as layouts]
   [layouter.excercise :as excercise]
   [layouter.gui :as gui]
   [layouter.key-log :as key-log]
   [layouter.keyboard :as keyboard]
   [layouter.keyboard-view :as keyboard-view]
   [layouter.layout :as layout]
   [layouter.optimization-progress-view :as optimization-progress-view]
   [layouter.optimize :as optimize]
   [layouter.rating :as rating]
   [layouter.text :as text]
   [layouter.view :as view]
   [medley.core :as medley]
   [medley.core :as medley])
  (:import
   java.util.Locale))




(defn on-click [handler node]
  {:node node
   :mouse-event-handler (fn [_node event]
                          (when (= :mouse-clicked (:type event))
                            (handler))
                          event)})

(defn- heat-map-shade [propability largest-character-propability]
  (int (* 255
          (/ propability
             largest-character-propability
             2))))

(deftest test-heat-map-shade
  (is (= 0.0
         (float (heat-map-shade 0
                                1))))

  (is (= 31
         (heat-map-shade 1
                         4)))

  (is (= 63
         (heat-map-shade 1
                         2)))

  (is (= 127
         (heat-map-shade 1
                         1))))

(comment
  (heat-map-shade (apply min (vals (:character-distribution hybrid-statistics)))
                  (apply max (vals (:character-distribution hybrid-statistics))))
  )


(defn format-in-us-locale [format & arguments]
  (String/format Locale/US
                 format
                 (to-array arguments)))




(defn mix-numbers [ratio number-1 number-2]
  (+ (* (- 1 ratio) number-1 )
     (* ratio number-2)))

(deftest test-mix-numbers
  (is (= 1
         (mix-numbers 0 1 2)))

  (is (= 2
         (mix-numbers 1 1 2)))

  (is (= 1.5
         (mix-numbers 0.5 1 2))))

(defn mix-colors [ratio color-1 color-2]
  (vec (map (partial mix-numbers ratio)
            color-1
            color-2)))

(deftest test-mix-colors
  (is (= [1.0 1.5 2.0 2.5]
         (mix-colors 0.5
                     [1 2 3 4]
                     [1 1 1 1]))))


(defn mix-numbers [ratio number-1 number-2]
  (+ (* (- 1 ratio) number-1 )
     (* ratio number-2)))

(deftest test-mix-numbers
  (is (= 1
         (mix-numbers 0 1 2)))

  (is (= 2
         (mix-numbers 1 1 2)))

  (is (= 1.5
         (mix-numbers 0.5 1 2))))

(defn mix-colors [ratio color-1 color-2]
  (vec (map (partial mix-numbers ratio)
            color-1
            color-2)))

(deftest test-mix-colors
  (is (= [1.0 1.5 2.0 2.5]
         (mix-colors 0.5
                     [1 2 3 4]
                     [1 1 1 1]))))

(defn key-heat-map-view [_cocoa-key-code-to-character _character-to-cocoa-key-code _character-distribution]
  (let [state-atom (dependable-atom/atom {})]
    (fn [cocoa-key-code-to-character character-to-cocoa-key-code character-distribution]
      (layouts/vertically-2 {:margin 10}
                            (keyboard-view/keyboard-view
                             cocoa-key-code-to-character
                             (let [largest-character-propability (apply max (vals character-distribution))]
                               (into {}
                                     (map (fn [[character propability]]
                                            [(character-to-cocoa-key-code character)
                                             [255 255 255 (heat-map-shade propability largest-character-propability)]])
                                          character-distribution))
                               #_(medley/map-kv-vals (fn [cocoa-key-code color]
                                                       (vec (concat (take 3 color)
                                                                    [(* 255
                                                                        (/ (or (get character-distribution
                                                                                    (cocoa-key-code-to-character cocoa-key-code))
                                                                               0)
                                                                           largest-character-propability))])))
                                                     keyboard-view/key-colors-for-fingers)
                               #_(merge-with (fn [finger-color character-propability]
                                               (mix-colors (/ character-propability
                                                              largest-character-propability)
                                                           (gui/multiply-color 0.0 finger-color)
                                                           [200 100 100 255]))
                                             keyboard-view/key-colors-for-fingers
                                             (medley/map-keys character-to-cocoa-key-code
                                                              character-distribution)))
                             {:on-key-event (fn [event]
                                              (case (:type event)
                                                :mouse-entered-character
                                                (swap! state-atom assoc :character-under-mouse (:character event))
                                                :mouse-left-character
                                                (swap! state-atom dissoc :character-under-mouse (:character event))
                                                nil))})
                            (gui/text (if-let [character-under-mouse (:character-under-mouse @state-atom)]
                                        (format-in-us-locale "%.3f" (get character-distribution
                                                                         character-under-mouse))
                                        ""))))))




(comment
  (start-view (fn []
                [#'key-heat-map-view (first @optimized-layouts-atom)
                 (:character-distribution hybrid-statistics)]))

  (let [character-distribution (:character-distribution hybrid-statistics)
        largest-character-propability (apply max (vals character-distribution))]
    (sort-by second (medley/map-vals #(/ % largest-character-propability)
                                     character-distribution)))
  )



(defn- ngram-view [cocoa-key-code-to-character character-to-cocoa-key-code n-gram]
  (let [key-highlight-color (into {}
                                  (concat (map-indexed (fn [index character]
                                                         [(character-to-cocoa-key-code character)
                                                          (gui/multiply-color (- 1 (* 0.3 index))
                                                                              [140 140 255 255])])

                                                       n-gram)))
        rating (rating/rate-n-gram-roll (map (comp keyboard/cocoa-key-code-to-key
                                                   character-to-cocoa-key-code)
                                             n-gram))
        key-colors-for-fingers (medley/map-vals (partial gui/multiply-color
                                                         (max 0.4 (- 1 (:effort rating))))
                                                keyboard-view/key-colors-for-fingers)]
    (layouts/vertically-2 {} (gui/text (str (:effort rating) " " (apply str n-gram)))
                          [keyboard-view/keyboard-view
                           cocoa-key-code-to-character
                           (merge keyboard-view/key-colors-for-fingers
                                  key-highlight-color)])))

(defn n-gram-comparison-view [named-layout _n-gram-distribution]
  (let [cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character (:layout named-layout))
        character-to-cocoa-key-code (layout/layout-to-character-to-cocoa-key-code (:layout named-layout))]
    (fn [named-layout n-gram-distribution]
      (layouts/vertically-2 {:margin 10}
                            (gui/text (pr-str (:multipliers named-layout)))
                            (layouts/flow (for [n-gram (map first (take 50 (reverse (sort-by second n-gram-distribution))))]
                                            (layouts/with-margin 20 (ngram-view cocoa-key-code-to-character
                                                                                character-to-cocoa-key-code
                                                                                n-gram))))))))

(comment
  (start-view (fn []
                [#'n-gram-comparison-view (first @optimized-layouts-atom)
                 (:digram-distribution hybrid-statistics)]))
  )



(defn merge-summary [summary]
  (apply merge
         (select-keys summary [:total])
         (:holistic-ratings summary)
         (vals (dissoc summary :total :holistic-ratings))))

(deftest test-merge-summary
  (is (= {:total 1.65,
          :hand-balance 0.6666666666666666,
          :vertical-movement 0.0,
          :horizontal-movement 0.0,
          :2-roll 0.5,
          :3-roll 0.3333333333333333,
          :finger-type 0.15000000000000002,
          :key 0.0}
         (merge-summary {:digrams {:vertical-movement 0.0, :horizontal-movement 0.0, :2-roll 0.5},
                         :trigrams {:3-roll 0.3333333333333333},
                         :characters {:finger-type 0.15000000000000002, :key 0.0},
                         :total 1.65,
                         :holistic-ratings {:hand-balance 0.6666666666666666}}))))
(defn cell [content]
  (layouts/with-margin 10 (layouts/with-maximum-size nil 20 (layouts/center-vertically content))))

(defn rating-description-to-row [rating-description]
  (concat [(string/join "" (first (:propability rating-description)))]
          (apply concat
                 (for [rating (:ratings rating-description)]
                   [(name (:label rating))
                    (:effort rating)]))))

(deftest test-rating-description-to-row
  (is (= '("[[\"e\" \"l\"] 0.25]"
           :different-hand
           0
           :different-finger
           0
           :no-roll
           -1)
         (rating-description-to-row '{:propability [["e" "l"] 0.25],
                                      :ratings
                                      [{:label :different-hand, :effort 0, :rating :vertical-movement}
                                       {:label :different-finger,
                                        :effort 0,
                                        :rating :horizontal-movement}
                                       {:label :no-roll, :effort -1, :rating :roll}]}))))

(defn rating-description-to-rows [rating-description]
  (apply concat
         (interpose [[]]
                    (for [aspect [:digrams :characters]]
                      (concat [(concat ["value"]
                                       (interpose ""
                                                  (map :rating (:ratings (first (get rating-description aspect))))))]
                              (map rating-description-to-row
                                   (get rating-description aspect)))))))

(deftest test-rating-description-to-rows
  (is (= '((:propability :vertical-movement "" :horizontal-movement "" :roll)
           ("[\"e\" \"l\"] 0,25000" "different-hand" 0 "different-finger" 0 "no-roll" -1)
           ("[\"l\" \"l\"] 0,25000" "same-row" 0 "same-column" 0 "no-roll" -1)
           []
           (:propability :finger-type "" :key)
           ("l 0,40000" "middle" -1/4 "regular" 0.5)
           ("o 0,20000" "index" 0 "regular" 0.5))
         (rating-description-to-rows '{:digrams
                                       ({:propability [["e" "l"] 0.25],
                                         :ratings
                                         [{:label :different-hand, :effort 0, :rating :vertical-movement}
                                          {:label :different-finger,
                                           :effort 0,
                                           :rating :horizontal-movement}
                                          {:label :no-roll, :effort -1, :rating :roll}]}
                                        {:propability [["l" "l"] 0.25],
                                         :ratings
                                         [{:label :same-row, :effort 0, :rating :vertical-movement}
                                          {:label :same-column, :effort 0, :rating :horizontal-movement}
                                          {:label :no-roll, :effort -1, :rating :roll}]}),
                                       :characters
                                       ({:propability ["l" 0.4],
                                         :ratings
                                         [{:effort -1/4, :label :middle, :rating :finger-type}
                                          {:rating :key, :label :regular :effort 0.5}]}
                                        {:propability ["o" 0.2],
                                         :ratings
                                         [{:effort 0, :label :index, :rating :finger-type}
                                          {:rating :key, :label :regular :effort 0.5}]})}))))

(defn rating-description-table [state-atom rating-to-aspect rating-description rating]
  (layouts/grid (let [aspect (rating-to-aspect rating)]
                  (doall (concat [(map (fn [value]
                                         (cell (visuals/text (str value))))
                                       (concat ["value"]
                                               (interpose ""
                                                          (map :rating (:ratings (first (get rating-description aspect)))))))]
                                 (map (fn [rating-description]
                                        (let [[value & ratings] (rating-description-to-row rating-description)]
                                          (concat [{:node (cell (visuals/text value))
                                                    :mouse-event-handler (fn [node event]
                                                                           (when (= :nodes-under-mouse-changed (:type event))
                                                                             (if (contains? (set (map :id (:nodes-under-mouse event)))
                                                                                            (:id node))
                                                                               (swap! state-atom assoc :highlighted-characters (set (let [value (first (:propability rating-description))]
                                                                                                                                      (if (string? value)
                                                                                                                                        [value]
                                                                                                                                        value))))
                                                                               (swap! state-atom assoc :highlighted-characters #{})))
                                                                           event)}]
                                                  (map (fn [value]
                                                         (cell (visuals/text (str value))))
                                                       ratings))))
                                      (take 30 (get rating-description aspect))))))))

(defn distribtion-rating-description-view [layout _rating distribution-rating-description]
  (let [state-atom (dependable-atom/atom {:highlighted-characters #{}})
        cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character (:layout layout))
        character-to-cocoa-key-code (layout/layout-to-character-to-cocoa-key-code (:layout layout))
        rating-to-aspect (into {} (for [aspect (keys distribution-rating-description)
                                        rating (map :rating (:ratings (first (get distribution-rating-description aspect))))]
                                    [rating aspect]))]
    (fn [_layout rating distribution-rating-description]
      (let [key-color (into {}
                            (concat (for [highlighted-character (:highlighted-characters @state-atom)]
                                      [(character-to-cocoa-key-code highlighted-character) [100 150 70 255]])))]
        (layouts/vertically-2 {:margin 10}
                              ;; (text (:name layout))
                              (gui/text (pr-str (:multipliers layout)))
                              [keyboard-view/keyboard-view cocoa-key-code-to-character
                               (merge keyboard-view/key-colors-for-fingers
                                      key-color)]
                              [rating-description-table state-atom rating-to-aspect distribution-rating-description rating])))))

(defn describe-layout-rating [text-statistics layout]
  (let [character-to-key (comp keyboard/cocoa-key-code-to-key
                               (layout/layout-to-character-to-cocoa-key-code layout))]
    {:distributions {:digrams (doall (for [digram-propability (->> (:digram-distribution text-statistics)
                                                                   (sort-by second)
                                                                   (reverse))]
                                       {:propability digram-propability
                                        :ratings (rating/rate-key-pair (map character-to-key
                                                                            (first digram-propability)))}))
                     :trigrams (doall (for [trigram-propability (->> (:trigram-distribution text-statistics)
                                                                     (sort-by second)
                                                                     (reverse))]
                                        {:propability trigram-propability
                                         :ratings (rating/rate-key-triple (map character-to-key
                                                                               (first trigram-propability)))}))
                     :characters (doall (for [character-propability (->> (:character-distribution text-statistics)
                                                                         (sort-by second)
                                                                         (reverse))]
                                          {:propability character-propability
                                           :ratings (rating/rate-key (character-to-key (first character-propability)))}))}
     :holistic-ratings {:hand-balance (* (rating/multiplier :hand-balance)
                                         (rating/rate-hand-balance (:character-distribution text-statistics)
                                                                   character-to-key))
                        :dist-from-colemak (rating/distance-from-colemak layout)}}))

(deftest test-describe-layout-rating
  (is (= '{:distributions
           {:digrams
            ({:propability [("e" "l") 0.25],
              :ratings
              [{:label :different-hand, :effort 0, :rating :vertical-movement}
               {:label :different-finger,
                :effort 0,
                :rating :horizontal-movement}
               {:rating :2-roll, :effort 1, :label :no-roll}]}
             {:propability [("l" "l") 0.25],
              :ratings
              [{:label :same-row, :effort 0, :rating :vertical-movement}
               {:label :same-column, :effort 0, :rating :horizontal-movement}
               {:rating :2-roll, :effort 1, :label :no-roll}]}
             {:propability [("l" "o") 0.25],
              :ratings
              [{:label :same-finger-one-row-leap,
                :effort 0.75,
                :rating :vertical-movement}
               {:label :same-column, :effort 0, :rating :horizontal-movement}
               {:rating :2-roll, :effort 1, :label :no-roll}]}),
            :trigrams
            ({:propability [("l" "l" "o") 0.3333333333333333],
              :ratings [{:rating :3-roll, :effort 1, :label :no-roll}]}),
            :characters
            ({:propability ["l" 0.4],
              :ratings
              [{:effort 0.5, :label :ring, :rating :finger-type}
               {:rating :key, :label :home, :effort 0}]}
             {:propability ["o" 0.2],
              :ratings
              [{:effort 0.5, :label :ring, :rating :finger-type}
               {:rating :key, :label :regular, :effort 0.25}]}
             {:propability ["e" 0.2],
              :ratings
              [{:effort 0.5, :label :ring, :rating :finger-type}
               {:rating :key, :label :regular, :effort 0.25}]}
             {:propability ["h" 0.2],
              :ratings
              [{:effort 0, :label :index, :rating :finger-type}
               {:rating :key, :label :regular, :effort 0.25}]})},
           :holistic-ratings {:hand-balance 0.75, :dist-from-colemak 1}}
         (describe-layout-rating (text/text-statistics "hello")
                                 layout/qwerty)))

  (is (= '{:distributions
           {:digrams
            ({:propability [("e" "l") 0.25],
              :ratings
              [{:label :different-hand, :effort 0, :rating :vertical-movement}
               {:label :different-finger,
                :effort 0,
                :rating :horizontal-movement}
               {:rating :2-roll, :effort 1, :label :no-roll}]}
             {:propability [("l" "l") 0.25],
              :ratings
              [{:label :same-row, :effort 0, :rating :vertical-movement}
               {:label :same-column, :effort 0, :rating :horizontal-movement}
               {:rating :2-roll, :effort 1, :label :no-roll}]}
             {:propability [("l" "o") 0.25],
              :ratings
              [{:label :same-row, :effort 0, :rating :vertical-movement}
               {:label :different-finger,
                :effort 0,
                :rating :horizontal-movement}
               {:rating :2-roll,
                :effort 0.0,
                :label :adjacent-invards-home-row-roll}]}),
            :trigrams
            ({:propability [("l" "l" "o") 0.3333333333333333],
              :ratings [{:rating :3-roll, :effort 1, :label :no-roll}]}),
            :characters
            ({:propability ["l" 0.4],
              :ratings
              [{:effort 0.25, :label :middle, :rating :finger-type}
               {:rating :key, :label :home, :effort 0}]}
             {:propability ["o" 0.2],
              :ratings
              [{:effort 0, :label :index, :rating :finger-type}
               {:rating :key, :label :home, :effort 0}]}
             {:propability ["e" 0.2],
              :ratings
              [{:effort 0, :label :index, :rating :finger-type}
               {:rating :key, :label :home, :effort 0}]}
             {:propability ["h" 0.2],
              :ratings
              [{:effort 0.25, :label :middle, :rating :finger-type}
               {:rating :key, :label :home, :effort 0}]})},
           :holistic-ratings
           {:hand-balance 0.33333333333333337, :dist-from-colemak 1}}
         (describe-layout-rating (text/text-statistics "hello")
                                 (layout/layout-from-qwerty {"h" "k"
                                                             "e" "j"
                                                             "l" "d"
                                                             "o" "f"})))))

(defn summarize-rating-aspect [rating-description]
  (apply merge-with +
         (for [row rating-description]
           (let [[_value propability] (:propability row)]
             (into {} (for [rating (:ratings row)]
                        [(:rating rating)
                         (* propability (:effort rating))]))))))

(deftest test-summarize-rating-aspect
  (is (= {:vertical-movement 0.0, :horizontal-movement 0.0, :roll -0.5}
         (summarize-rating-aspect '({:propability [["e" "l"] 0.25],
                                     :ratings
                                     [{:label :different-hand, :effort 0, :rating :vertical-movement}
                                      {:label :different-finger,
                                       :effort 0,
                                       :rating :horizontal-movement}
                                      {:label :no-roll, :effort -1, :rating :roll}]}
                                    {:propability [["l" "l"] 0.25],
                                     :ratings
                                     [{:label :same-row, :effort 0, :rating :vertical-movement}
                                      {:label :same-column, :effort 0, :rating :horizontal-movement}
                                      {:label :no-roll, :effort -1, :rating :roll}]}
                                    {:propability [["l" "o"] 0.25],
                                     :ratings
                                     [{:label :same-row, :effort 0, :rating :vertical-movement}
                                      {:label :different-finger,
                                       :effort 0,
                                       :rating :horizontal-movement}
                                      {:label :invards-roll-on-home-row, :effort 0, :rating :roll}]})))))

(defn summarize-rating-description [rating-description]
  (let [summary (medley/map-vals summarize-rating-aspect
                                 (:distributions rating-description))]
    (assoc summary
           ;; :total (->> (conj (vals summary)
           ;;                   (:holistic-ratings rating-description))
           ;;             (map (fn [ratings]
           ;;                    (reduce + (vals ratings))))
           ;;             (reduce +))
           :holistic-ratings (:holistic-ratings rating-description))))

(deftest test-summarize-rating-description
  (is (= {:digrams
          {:vertical-movement 0.0, :horizontal-movement 0.0, :2-roll 0.5},
          :trigrams {:3-roll 0.3333333333333333},
          :characters {:finger-type 0.15000000000000002, :key 0.0},
          :total 2.65,
          :holistic-ratings
          {:hand-balance 0.6666666666666666, :dist-from-colemak 1}}
         (summarize-rating-description '{:distributions
                                         {:digrams
                                          ({:propability [("e" "l") 0.25],
                                            :ratings
                                            [{:label :different-hand, :effort 0, :rating :vertical-movement}
                                             {:label :different-finger,
                                              :effort 0,
                                              :rating :horizontal-movement}
                                             {:rating :2-roll, :effort 1, :label :no-roll}]}
                                           {:propability [("l" "l") 0.25],
                                            :ratings
                                            [{:label :same-row, :effort 0, :rating :vertical-movement}
                                             {:label :same-column, :effort 0, :rating :horizontal-movement}
                                             {:rating :2-roll, :effort 1, :label :no-roll}]}
                                           {:propability [("l" "o") 0.25],
                                            :ratings
                                            [{:label :same-row, :effort 0, :rating :vertical-movement}
                                             {:label :different-finger,
                                              :effort 0,
                                              :rating :horizontal-movement}
                                             {:rating :2-roll,
                                              :effort 0.0,
                                              :label :adjacent-invards-home-row-roll}]}),
                                          :trigrams
                                          ({:propability [("l" "l" "o") 0.3333333333333333],
                                            :ratings [{:rating :3-roll, :effort 1, :label :no-roll}]}),
                                          :characters
                                          ({:propability ["l" 0.4],
                                            :ratings
                                            [{:effort 0.25, :label :middle, :rating :finger-type}
                                             {:rating :key, :label :home, :effort 0}]}
                                           {:propability ["o" 0.2],
                                            :ratings
                                            [{:effort 0, :label :index, :rating :finger-type}
                                             {:rating :key, :label :home, :effort 0}]}
                                           {:propability ["e" 0.2],
                                            :ratings
                                            [{:effort 0, :label :index, :rating :finger-type}
                                             {:rating :key, :label :home, :effort 0}]}
                                           {:propability ["h" 0.2],
                                            :ratings
                                            [{:effort 0.25, :label :middle, :rating :finger-type}
                                             {:rating :key, :label :home, :effort 0}]})},
                                         :holistic-ratings {:hand-balance 0.6666666666666666 :dist-from-colemak 1}}))))

(def multiplier-groups [[:roll [:digram-roll
                                :trigram-roll]]

                        [:key [:key-rating
                               :finger-type]]

                        [:movement [:horizontal-movement
                                    :vertical-movement
                                    :vertical-movement-in-skipgram]]

                        [:hand [:hand-balance
                                :hand-alternation]]])

(def multiplier-key-to-group (into {} (apply concat
                                             (for [[group-key multiplier-keys] multiplier-groups]
                                               (for [multiplier-key multiplier-keys]
                                                 [multiplier-key group-key])))))

(def group-key-to-color (gui/create-key-to-color (map first multiplier-groups)))

(defn- multiplier-groups-view [multipliers multiplier-groups]
  (layouts/with-minimum-size 50 nil
    (layouts/vertically-2 {:margin 1
                           :fill-width? false}
                          (for [[group-key multiplier-keys] multiplier-groups]
                            (for [multiplier-key multiplier-keys]
                              (assoc (visuals/rectangle-2 {:fill-color (group-key-to-color group-key)})
                                     :height 10
                                     :width (* 50
                                               (or (get multipliers
                                                        multiplier-key)
                                                   0))))))))

(defn multipliers-view [_multipliers]
  (let [state-atom (dependable-atom/atom {})]
    (fn [multipliers]
      (layouts/vertically-2 {:margin 0}
                            {:node (gui/box (layouts/horizontally-2 {:margin 1}
                                                                    (map (partial multiplier-groups-view multipliers)
                                                                         (map vector multiplier-groups))
                                                                    ;; (multiplier-groups-view multipliers (take 2 multiplier-groups))
                                                                    ;; (multiplier-groups-view multipliers (drop 2 multiplier-groups))
                                                                    )
                                            {:padding 0
                                             :fill-color nil
                                             :draw-color [0.4 0.4 0.4 1.0]
                                             :corner-arc-radius 0})
                             :mouse-event-handler (fn [node event]
                                                    (when (= :nodes-under-mouse-changed (:type event))
                                                      (swap! state-atom assoc :mouse-over? (contains? (set (map :id (:nodes-under-mouse event)))
                                                                                                      (:id node))))

                                                    event)}

                            (when (:mouse-over? @state-atom)
                              (layouts/hover (gui/box (layouts/vertically-2 {:margin 10}
                                                                            (for [multiplier-key (mapcat second multiplier-groups)]
                                                                              (gui/text (str multiplier-key " " (multiplier-key multipliers))
                                                                                        {:color (group-key-to-color (multiplier-key-to-group multiplier-key))})))
                                                      {:padding 10
                                                       :fill-color [0 0 0 0.8]})))))))

(comment
  (view/start-view (fn [] (gui/black-background [#'multipliers-view {:key-rating 1
                                                                     :vertical-movement-in-skipgram 1,
                                                                     :vertical-movement 1,
                                                                     :trigram-roll 0.0,
                                                                     :hand-balance 0.1,
                                                                     :hand-alternation 1,
                                                                     :dist-from-colemak 0.0,
                                                                     :finger-type 0.1,
                                                                     :digram-roll 0.0,
                                                                     :horizontal-movement 1}]))
                   {:join? true})
  )


(defn layout-rating-comparison-view [_statistics _layouts]
  (let [state-atom (dependable-atom/atom {})]
    (fn [statistics layouts]
      (if-let [selected-layout-rating-description (:selected-layout-rating-description @state-atom)]
        (on-click (fn []
                    (swap! state-atom dissoc :selected-layout-rating-description))
                  (cond (= :2-roll (:rating @state-atom))
                        [n-gram-comparison-view
                         (:layout @state-atom)
                         (:digram-distribution statistics)]

                        (= :3-roll (:rating @state-atom))
                        [n-gram-comparison-view
                         (:layout @state-atom)
                         (:trigram-distribution statistics)]

                        :else
                        [distribtion-rating-description-view
                         (:layout @state-atom)
                         (:rating @state-atom)
                         (:distributions selected-layout-rating-description)]))
        (let [layouts (for [layout layouts]
                        (let [layout-rating-description (describe-layout-rating statistics
                                                                                (:layout layout))
                              summary (summarize-rating-description layout-rating-description)]
                          (-> layout
                              (assoc :layout-rating-description layout-rating-description
                                     :summary (-> summary
                                                  (merge-summary)
                                                  (assoc :rating-wo-di (- (rating/rate-layout statistics (:layout layout))
                                                                          (-> summary :holistic-ratings :dist-from-colemak))))))))
              columns (for [column (into #{} (apply concat (map keys (map :summary layouts))))]
                        {:key column
                         :minimum (apply min (map column (map :summary layouts)))
                         :maximum (apply max (map column (map :summary layouts)))})]
          [layouts/grid (doall (concat [(concat [(gui/text "layout")]
                                                (for [column columns]
                                                  (on-click (fn []
                                                              (if (= column (:sort-column @state-atom))
                                                                (swap! state-atom update :sort-descending? not)
                                                                (swap! state-atom assoc :sort-column column)))
                                                            (cell (gui/text (name (:key column)))))))]
                                       (for [layout (-> (if (:sort-column @state-atom)
                                                          (sort-by (fn [layout]
                                                                     (get (:summary layout)
                                                                          (:key (or (:sort-column @state-atom)
                                                                                    (first columns)))))
                                                                   layouts)
                                                          layouts)
                                                        (cond-> (:sort-descending? @state-atom)
                                                          (reverse)))]
                                         (concat [(layouts/horizontally-2 {:margin 10}
                                                                          [multipliers-view (:multipliers layout)]
                                                                          (gui/text (layout/layout-name layout)))]
                                                 ;; hot-right-now TODO: remove me
                                                 (for [column columns]
                                                   (on-click (fn []
                                                               (swap! state-atom
                                                                      assoc
                                                                      :selected-layout-rating-description (:layout-rating-description layout)
                                                                      :rating (:key column)
                                                                      :layout layout))
                                                             (cell (layouts/superimpose (assoc (visuals/rectangle-2 {:fill-color [100 100 100 255]})
                                                                                               :height gui/font-size
                                                                                               :width (* 200
                                                                                                         (abs (/ (get (:summary layout)
                                                                                                                      (:key column))
                                                                                                                 (max 0.001
                                                                                                                      (:maximum column)))))
                                                                                               #_(let [offset (* 0.99 (:minimum column))]
                                                                                                   (* 300
                                                                                                      (abs (/ (- (get (:summary layout)
                                                                                                                      (:key column))
                                                                                                                 offset)
                                                                                                              (- (:maximum column)
                                                                                                                 offset))))))
                                                                                        (gui/text (str (format "%.4f" (get (:summary layout)
                                                                                                                           (:key column)))))))))))))])))))


(defn named-layout-atom-to-named-layout [named-layout-atom]
  (-> named-layout-atom
      (assoc :layout @(:layout-atom named-layout-atom))
      (dissoc :layout-atom)))

(defn named-layout-to-named-layout-atom [named-layout]
  (-> named-layout
      (assoc :layout-atom (dependable-atom/atom (:layout named-layout)))
      (dissoc :layout)))

(defn n-gram-flow [statistics cocoa-key-code-to-characters character-to-cocoa-key-codes]
  (layouts/flow (for [n-gram (map first (take 3 (reverse (sort-by second (:digram-distribution statistics)))))]
                  (layouts/with-margin 40
                    (layouts/vertically-2 {:margin 10}
                                          (ngram-view (first cocoa-key-code-to-characters)
                                                      (first character-to-cocoa-key-codes)
                                                      n-gram)
                                          (ngram-view (second cocoa-key-code-to-characters)
                                                      (second character-to-cocoa-key-codes)
                                                      n-gram))))))


(defn layout-editor [_layout-atom _key-colors _statistics]
  (let [state-atom (dependable-atom/atom {})]
    (fn [layout-atom key-colors statistics]
      (let [cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character @layout-atom)
            character-to-cocoa-key-code (layout/layout-to-character-to-cocoa-key-code @layout-atom)
            state @state-atom
            current-effort (rating/rate-layout statistics
                                               @layout-atom)
            on-key-event (fn [event]
                           (case (:type event)
                             :mouse-entered-character
                             (swap! state-atom assoc :character-under-mouse (:character event))
                             :mouse-left-character
                             (swap! state-atom dissoc :character-under-mouse (:character event))
                             :mouse-pressed
                             (if-let [selected-character (:selected-character state)]
                               (do (swap! layout-atom
                                          optimize/swap-mappings
                                          (medley/find-first (fn [mapping]
                                                               (= selected-character
                                                                  (:character mapping)))
                                                             @layout-atom)
                                          (medley/find-first (fn [mapping]
                                                               (= (:character event)
                                                                  (:character mapping)))
                                                             @layout-atom))
                                   (swap! state-atom dissoc :selected-character))
                               (swap! state-atom assoc :selected-character (:character event)))))]
        {:node
         (if-let [selected-character (:selected-character state)]
           (let [selected-mapping (medley/find-first (fn [mapping]
                                                       (= selected-character
                                                          (:character mapping)))
                                                     @layout-atom)
                 cocoa-key-code-to-effort (into {}
                                                (for [mapping (remove #{selected-mapping}
                                                                      @layout-atom)]
                                                  [(:cocoa-key-code mapping)
                                                   (rating/rate-layout statistics
                                                                       (optimize/swap-mappings @layout-atom
                                                                                               selected-mapping
                                                                                               mapping))]))
                 all-efforts (conj (vals cocoa-key-code-to-effort)
                                   current-effort)
                 maximum-effort (apply max all-efforts)
                 minimum-effort (apply min all-efforts)]
             (layouts/vertically-2 {}
                                   (keyboard-view/keyboard-view
                                    cocoa-key-code-to-character
                                    (merge keyboard-view/key-colors-for-fingers
                                           (medley/map-vals (fn [effort]
                                                              (if (< effort current-effort)
                                                                [155 255 155 (* 255
                                                                                (min 1
                                                                                     (/ (- current-effort effort)
                                                                                        (max 0.001
                                                                                             (- current-effort minimum-effort)))))]
                                                                [255 155 155 (* 255
                                                                                (min 1
                                                                                     (/ (- effort current-effort)
                                                                                        (max 0.001
                                                                                             (- maximum-effort current-effort)))))]))
                                                            cocoa-key-code-to-effort)
                                           {(character-to-cocoa-key-code selected-character) [120 120 200 255]})
                                    {:on-key-event on-key-event})
                                   (gui/text (string/join " "
                                                          [ ;; "effort: "
                                                           (format-in-us-locale "%.3f" current-effort)
                                                           ;; " difference to minimum effort "
                                                           (format-in-us-locale "%.3f" (* 100
                                                                                          (/ (- minimum-effort current-effort)
                                                                                             current-effort)))
                                                           ;; " difference to maximum effort "
                                                           (format-in-us-locale "%.3f" (* 100 (/ (- maximum-effort current-effort)
                                                                                                 current-effort)))

                                                           (when-let [character-under-mouse (:character-under-mouse state)]
                                                             (when-let [effort-after-swap (get cocoa-key-code-to-effort
                                                                                               (character-to-cocoa-key-code character-under-mouse))]
                                                               (format-in-us-locale "%.3f" (* 100
                                                                                              (/ (- effort-after-swap
                                                                                                    current-effort)
                                                                                                 current-effort)))))]))))
           (layouts/vertically-2 {}
                                 [keyboard-view/keyboard-view
                                  cocoa-key-code-to-character
                                  (merge keyboard-view/key-colors-for-fingers
                                         key-colors)
                                  {:on-key-event on-key-event}]
                                 (gui/text (str "effort: " (format-in-us-locale "%.3f" current-effort)))))


         :can-gain-focus? true}))))

(defn- differing-cocoa-keycodes [named-layout-1 named-layout-2]
  (map first
       (set/difference
        (set (-> named-layout-1
                 :layout
                 layout/layout-to-cocoa-key-code-to-character))
        (set (-> named-layout-2
                 :layout
                 layout/layout-to-cocoa-key-code-to-character)))))

(defn- differing-key-color-mapping [named-layout-1 named-layout-2]
  (into {}
        (for [differing-cocoa-keycode (differing-cocoa-keycodes named-layout-1
                                                                named-layout-2)]
          [differing-cocoa-keycode [0.5 0.2 0.2 1.0]])))


(defn layout-comparison-view [named-layout-atoms statistics]
  (let [named-layouts (for [named-layout-atom named-layout-atoms]
                        (assoc (named-layout-atom-to-named-layout named-layout-atom)
                               :layout-rating-description (describe-layout-rating statistics
                                                                                  @(:layout-atom named-layout-atom))))
        cocoa-key-code-to-characters (map layout/layout-to-cocoa-key-code-to-character (map :layout named-layouts))
        character-to-cocoa-key-codes (map layout/layout-to-character-to-cocoa-key-code (map :layout named-layouts))]
    (layouts/superimpose (visuals/rectangle-2 {:fill-color [0 0 0 255]})
                         (layouts/vertically-2 {:margin 10}
                                               #_(layouts/with-margins 50 0 0 50 [layout-rating-comparison-view statistics named-layouts])
                                               (layouts/flow (layouts/with-margin 40
                                                               (layouts/vertically-2 {:margin 10}
                                                                                     (gui/text "editor")
                                                                                     (for [[named-layout-atom-1 named-layout-atom-2] (partition-all 2 1 named-layout-atoms)]
                                                                                       (layouts/vertically-2 {:margin 10}
                                                                                                             (gui/text (:name named-layout-atom-1))
                                                                                                             [layout-editor
                                                                                                              (:layout-atom named-layout-atom-1)
                                                                                                              (if (nil? named-layout-atom-2)
                                                                                                                {}
                                                                                                                (differing-key-color-mapping (named-layout-atom-to-named-layout named-layout-atom-1)
                                                                                                                                             (named-layout-atom-to-named-layout named-layout-atom-2)))
                                                                                                              statistics]))

                                                                                     #_[layout-editor
                                                                                        (:layout-atom (second named-layout-atoms))
                                                                                        (into {}
                                                                                              (for [differing-cocoa-keycode (map first (set/difference (set (second cocoa-key-code-to-characters))
                                                                                                                                                       (set (first cocoa-key-code-to-characters))))]
                                                                                                [differing-cocoa-keycode [100 150 100 255]]))
                                                                                        statistics]))

                                                             (layouts/with-margin 40
                                                               (layouts/vertically-2 {:margin 0}
                                                                                     (gui/text "heatmap")

                                                                                     (for [named-layout-atom named-layout-atoms]
                                                                                       (layouts/vertically-2 {:margin 10}
                                                                                                             (gui/text (:name named-layout-atom))
                                                                                                             [key-heat-map-view
                                                                                                              (-> named-layout-atom
                                                                                                                  named-layout-atom-to-named-layout
                                                                                                                  :layout
                                                                                                                  layout/layout-to-cocoa-key-code-to-character)
                                                                                                              (-> named-layout-atom
                                                                                                                  named-layout-atom-to-named-layout
                                                                                                                  :layout
                                                                                                                  layout/layout-to-character-to-cocoa-key-code)

                                                                                                              (:character-distribution statistics)])
                                                                                       )
                                                                                     #_[key-heat-map-view
                                                                                        (first cocoa-key-code-to-characters)
                                                                                        (first character-to-cocoa-key-codes)
                                                                                        (:character-distribution statistics)]
                                                                                     ;; (gui/text "")
                                                                                     #_[key-heat-map-view
                                                                                        (second cocoa-key-code-to-characters)
                                                                                        (second character-to-cocoa-key-codes)
                                                                                        (:character-distribution statistics)]))

                                                             #_(for [n-gram (map first (take 82 (reverse (sort-by second (:digram-distribution statistics)))))]
                                                                 (layouts/with-margin 40
                                                                   (layouts/vertically-2 {:margin 10}
                                                                                         (ngram-view (first cocoa-key-code-to-characters)
                                                                                                     (first character-to-cocoa-key-codes)
                                                                                                     n-gram)
                                                                                         (ngram-view (second cocoa-key-code-to-characters)
                                                                                                     (second character-to-cocoa-key-codes)
                                                                                                     n-gram)))))))))

(def the-best #{{:cocoa-key-code 40, :character "e"}
                {:cocoa-key-code 34, :character "a"}
                {:cocoa-key-code 38, :character "t"}
                {:cocoa-key-code 6, :character "ä"}
                {:cocoa-key-code 46, :character "d"}
                {:cocoa-key-code 31, :character "v"}
                {:cocoa-key-code 7, :character "u"}
                {:cocoa-key-code 4, :character "r"}
                {:cocoa-key-code 1, :character "o"}
                {:cocoa-key-code 13, :character "z"}
                {:cocoa-key-code 0, :character "k"}
                {:cocoa-key-code 14, :character "g"}
                {:cocoa-key-code 35, :character "å"}
                {:cocoa-key-code 41, :character "p"}
                {:cocoa-key-code 45, :character "m"}
                {:cocoa-key-code 39, :character "x"}
                {:cocoa-key-code 3, :character "n"}
                {:cocoa-key-code 17, :character "f"}
                {:cocoa-key-code 37, :character "h"}
                {:cocoa-key-code 33, :character "q"}
                {:cocoa-key-code 2, :character "i"}
                {:cocoa-key-code 5, :character "s"}
                {:cocoa-key-code 12, :character "ö"}
                {:cocoa-key-code 9, :character "w"}
                {:cocoa-key-code 11, :character "b"}
                {:cocoa-key-code 15, :character "y"}
                {:cocoa-key-code 8, :character "c"}
                {:cocoa-key-code 16, :character "j"}
                {:cocoa-key-code 32, :character "l"}})

(defn best-layouts-per-statistics-and-multipliers-with-names [number-of-layouts-per-group minimum-number-of-differing-keys]
  (->> (optimization-progress-view/best-layouts-per-statistics-and-multipliers number-of-layouts-per-group
                                                                               minimum-number-of-differing-keys
                                                                               @optimize/layout-optimization-log-atom)
       (map-indexed (fn [index named-layout]
                      (assoc named-layout :name (str "opt-" index))))))

(defn optimized-layouts-comparison-view []
  (let [named-layout-atoms (concat [(named-layout-to-named-layout-atom {:layout layout/qwerty
                                                                        :name "qwerty"})
                                    (named-layout-to-named-layout-atom {:layout layout/colemak-dh
                                                                        :name "colemak"})
                                    #_(named-layout-to-named-layout-atom {:layout the-best})]
                                   #_best-layouts-per-statistics-and-multipliers-with-names-atoms
                                   #_(->> optimization-progress-view/best-optimization-run
                                          (:ratings)
                                          (map first)
                                          (take 3)
                                          (map-indexed (fn [index layout]
                                                         {:layout layout
                                                          :name (str "optimized-" index)}))
                                          (map named-layout-to-named-layout-atom)))
        named-layouts (map named-layout-atom-to-named-layout named-layout-atoms)]
    (gui/black-background (layouts/vertically-2 {:margin 10}
                                                (gui/text "hybrid statistics")
                                                [layout-rating-comparison-view text/hybrid-statistics named-layouts]
                                                (gui/text "english statistics")
                                                [layout-rating-comparison-view text/english-statistics named-layouts]
                                                (gui/text "finnish statistics")
                                                [layout-rating-comparison-view text/finnish-statistics named-layouts]
                                                [#'layout-comparison-view named-layout-atoms text/hybrid-statistics]))))

(defn- optimization-status []
  (->> @optimize/layout-optimization-log-atom
       (map-indexed (fn [index state]
                      (assoc state :index index)))
       (group-by (juxt :text-statistics-name :multipliers))
       (medley/map-vals (fn [states]
                          (let [ratings (->> states
                                             (mapcat :ratings)
                                             (map second))]
                            {:number-of-optimization-runs (count states)
                             ;; :average-rating (/ (reduce + ratings)
                             ;;                    (count ratings))
                             ;; :min-rating (apply min ratings)
                             ;; :min-rating-run-number (->> states
                             ;;                             (map :ratings)
                             ;;                             (map optimize/best-rating)
                             ;;                             (map-indexed vector)
                             ;;                             (sort-by second)
                             ;;                             (first)
                             ;;                             (first))
                             :min-rating-runs (->> states
                                                   (sort-by :index)
                                                   (map :ratings)
                                                   (map optimize/best-rating)
                                                   (map-indexed vector)
                                                   (sort-by second)
                                                   (take 5))
                             :best-layout-homerow-string (->> states
                                                              (mapcat :ratings)
                                                              (sort-by second)
                                                              (first)
                                                              (first)
                                                              (layout/homerow-string))
                             ;; :max-rating (apply max ratings)
                             ;; :latest-ratings (->> states
                             ;;                      (take-last 5)
                             ;;                      (map :ratings)
                             ;;                      (map first)
                             ;;                      (map second))
                             })))))

(defn- min-rating-runs-graph [scale-points status]
  (let [graph-width 500
        graph-height 100]
    (when (not (empty? (:min-rating-runs status)))
      (layouts/with-margin 10
        (gui/box (layouts/with-minimum-size graph-width graph-height

                   (path/path [100 100 100 255]
                              5
                              (let [minimun-score (apply min (map second  (:min-rating-runs status)))]
                                (->> (:min-rating-runs status)
                                     (sort-by first)
                                     (map (fn [[generation score]]
                                            {:x generation
                                             :y (- score (* 0.98 minimun-score))}))
                                     (scale-points graph-width graph-height)
                                     (map (partial optimization-progress-view/scale-point {:x 1 :y -1}))
                                     (optimization-progress-view/move-to-origin)))))
                 {:padding 0
                  :fill-color nil
                  :draw-color [0.2 0.2 0.2 1.0]
                  :corner-arc-radius 0})))))


(defn optimizatin-status-view []
  (layouts/vertically-2 {:margin 10}
                        (gui/text (str "generation number: " (:generation-number (last @optimize/optimization-history-atom))))
                        (gui/text (str "best rating: " (when-some [ratings (:ratings (last @optimize/optimization-history-atom))]
                                                         (optimize/best-rating ratings))))
                        (for [[[text-statistics-name multipliers] status] (optimization-status)]
                          (layouts/vertically-2 {:margin 10}
                                                (gui/text (str text-statistics-name " " (layout/multipliers-to-layout-name multipliers)))
                                                (for [key (sort-by name (keys status))]
                                                  (gui/text (str key ": " (pr-str (key status)))))
                                                (layouts/horizontally-2 {:margin 10}
                                                                        (min-rating-runs-graph optimization-progress-view/scale-to-view-but-preserve-full-y-scale status)
                                                                        #_(min-rating-runs-graph optimization-progress-view/scale-to-view status))))))
(comment
  (optimization-status)
  (->> @optimize/layout-optimization-log-atom
       (take-last 5)
       (map :ratings)
       (map first)
       (map second))

  (first (:ratings (last @optimize/optimization-history-atom)))
  ) ;; TODO: remove me

(defn optimization-status-with-rating-comparison-view []
  (let [named-layouts (concat [{:layout layout/qwerty
                                :name "qwerty"}
                               {:layout layout/colemak-dh
                                :name "colemak"}]
                              (best-layouts-per-statistics-and-multipliers-with-names 1 0))]
    (gui/black-background (layouts/vertically-2 {:margin 10}
                                                (gui/text "hybrid statistics")
                                                [layout-rating-comparison-view text/hybrid-statistics named-layouts]
                                                (gui/text "english statistics")
                                                [layout-rating-comparison-view text/english-statistics named-layouts]
                                                (gui/text "finnish statistics")
                                                [layout-rating-comparison-view text/finnish-statistics named-layouts]
                                                [optimizatin-status-view]))))

(defn key-heat-map-view-for-named-layout [text-statistics named-layout]
  (layouts/vertically-2 {:margin 10}
                        [key-heat-map-view
                         (-> named-layout :layout layout/layout-to-cocoa-key-code-to-character)
                         (-> named-layout :layout layout/layout-to-character-to-cocoa-key-code)
                         (:character-distribution text-statistics named-layout)]
                        (gui/text (layout/layout-name named-layout))))

(defn scroll-pane [_content]
  (let [state-atom (dependable-atom/atom {:x 0 :y 0})]
    (fn [content]
      (layouts/transpose (:x @state-atom)
                         (:y @state-atom)
                         {:node content
                          ;; :x (:x @state-atom)
                          ;; :y (:y @state-atom)
                          :mouse-event-handler (fn [_node event]
                                                 (when (= :mouse-wheel-rotated
                                                          (:type event))
                                                   (swap! state-atom
                                                          update
                                                          (if (:horizontal? event)
                                                            :x :y)
                                                          (fn [value]
                                                            (min 0 (- value (* 5 (:precise-wheel-rotation event)))))))
                                                 event)}))))




(defonce edited-layout-atom (dependable-atom/atom layout/qwerty))

(defn optimized-layouts-comparison-view-2 []
  (let [english-demo-text (excercise/english-demo-text)
        finnish-demo-text (string/lower-case (string/replace (subs (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                                   5004 5100)
                                                             "\n" " "))

        state-atom (dependable-atom/atom {:selected-named-layout {:layout layout/qwerty
                                                                  :name "qwerty"}
                                          :selected-text-statistics text/keyboard-design-com-english-text-statistics

                                          :demo-text english-demo-text})
        ;;        random-layout (optimize/random-layout)
        ]
    (fn []
      (let [named-layouts (concat [{:layout layout/qwerty
                                    :name "qwerty"}
                                   {:layout layout/colemak-dh
                                    :name "colemak"}
                                   #_{:layout random-layout
                                      :name "random"}
                                   ;; hill-climbed-layout
                                   ;;last-layout
                                   ]
                                  (best-layouts-per-statistics-and-multipliers-with-names 1 0)
                                  #_(map (fn [named-layout]
                                           (update named-layout :layout (partial optimize/hill-climb-all text/keyboard-design-com-english-text-statistics)))
                                         (best-layouts-per-statistics-and-multipliers-with-names 1 0)))
            state @state-atom
            selected-named-layout (:selected-named-layout state)
            selected-text-statistics (:selected-text-statistics state)]
        (gui/black-background [scroll-pane (layouts/vertically-2 {:margin 10}
                                                                 (for [text-statistics [text/keyboard-design-com-english-text-statistics
                                                                                        text/wikinews-finnish-statistics

                                                                                        ;; text/hybrid-statistics-without-å
                                                                                        ;; text/english-statistics
                                                                                        ;; text/finnish-statistics-without-å
                                                                                        ;; (:fi key-log/statistics-from-key-log)
                                                                                        ;; (:en key-log/statistics-from-key-log)
                                                                                        ;; (:hybrid key-log/statistics-from-key-log)
                                                                                        ]]
                                                                   (layouts/vertically-2 {:margin 10}
                                                                                         (on-click (fn [] (swap! state-atom assoc
                                                                                                                 :selected-text-statistics text-statistics
                                                                                                                 :demo-text (if (= "kdc-en" (:name text-statistics))
                                                                                                                              english-demo-text
                                                                                                                              finnish-demo-text)))
                                                                                                   (gui/maby-highlight (= selected-text-statistics
                                                                                                                          text-statistics)
                                                                                                                       (gui/text (:name text-statistics))))
                                                                                         [layout-rating-comparison-view text-statistics (conj named-layouts
                                                                                                                                              {:layout @edited-layout-atom
                                                                                                                                               :name "edited"})]))
                                                                 (layouts/flow (for [named-layout named-layouts]
                                                                                 (layouts/vertically-2 {:margin 10}
                                                                                                       {:node (layouts/vertically-2 {:margin 10}
                                                                                                                                    (keyboard-view/keyboard-view (layout/layout-to-cocoa-key-code-to-character (:layout named-layout))
                                                                                                                                                                 (merge keyboard-view/key-colors-for-fingers
                                                                                                                                                                        (when selected-named-layout
                                                                                                                                                                          (differing-key-color-mapping selected-named-layout
                                                                                                                                                                                                       named-layout))))
                                                                                                                                    [multipliers-view (:multipliers named-layout)])
                                                                                                        :mouse-event-handler (fn [_node event]
                                                                                                                               (cond (and (= :mouse-clicked (:type event))
                                                                                                                                          (:shift event))
                                                                                                                                     (reset! edited-layout-atom (:layout named-layout))

                                                                                                                                     (= :mouse-clicked (:type event))
                                                                                                                                     (swap! state-atom assoc
                                                                                                                                            :selected-named-layout named-layout
                                                                                                                                            :previously-selected-named-layout selected-named-layout))
                                                                                                                               event)}
                                                                                                       (gui/text (layout/layout-name named-layout)))))
                                                                 (layouts/horizontally-2 {:margin 10}
                                                                                         {:node [excercise/layout-demo-view (:demo-text state)
                                                                                                 (:layout selected-named-layout)]
                                                                                          :local-id (take 5 (:demo-text state))}

                                                                                         [key-heat-map-view-for-named-layout selected-text-statistics selected-named-layout]

                                                                                         (when  (:previously-selected-named-layout state)
                                                                                           [key-heat-map-view-for-named-layout selected-text-statistics (:previously-selected-named-layout state)])

                                                                                         [key-heat-map-view-for-named-layout selected-text-statistics {:layout @edited-layout-atom
                                                                                                                                                       :name "edited"}]

                                                                                         (layouts/vertically-2 {:margin 20}
                                                                                                               [layout-editor
                                                                                                                edited-layout-atom
                                                                                                                (merge keyboard-view/key-colors-for-fingers
                                                                                                                       (when selected-named-layout
                                                                                                                         (differing-key-color-mapping selected-named-layout
                                                                                                                                                      {:layout @edited-layout-atom})))
                                                                                                                selected-text-statistics]
                                                                                                               (gui/text "editor")))
                                                                 (layouts/with-margins 50 0 0 0
                                                                   [optimizatin-status-view])
                                                                 (layouts/with-margins 50 0 0 0
                                                                   [#'optimization-progress-view/optimization-progress-view optimize/optimization-history-atom]))])))))

(defn text-statistics-view [highlighted-character on-mouse-enter on-mouse-leave text-statistics]
  (layouts/vertically-2 {:margin 10}
                        (gui/text (:name text-statistics))
                        (let [maximum (apply max (map second (-> text-statistics :character-distribution)))]
                          (for [[character frequency] (reverse (sort-by second (-> text-statistics :character-distribution)))]

                            {:node (layouts/horizontally-2 {:margin 10}
                                                           (layouts/with-minimum-size 150 0
                                                             (gui/text (str character " " (format-in-us-locale "%.3f" (* 100 frequency)))))
                                                           (cell (assoc (visuals/rectangle-2 {:fill-color (if (= highlighted-character character)
                                                                                                            [200 200 200 255]
                                                                                                            [100 100 100 255])})
                                                                        :height 30
                                                                        :width (* 100
                                                                                  (abs (/ frequency
                                                                                          (max 0.001
                                                                                               maximum)))))))
                             :mouse-event-handler (fn [node event]
                                                    (when (= :nodes-under-mouse-changed (:type event))
                                                      (if (contains? (set (map :id (:nodes-under-mouse event)))
                                                                     (:id node))
                                                        (on-mouse-enter character)
                                                        (on-mouse-leave character)))

                                                    event)})))
  )

(defn text-statistics-comparison-view []
  (let [state-atom (dependable-atom/atom {})
        on-mouse-enter (fn [character]
                         (swap! state-atom assoc :highlighted-character character))
        on-mouse-leave (fn [_character]
                         (swap! state-atom dissoc :highlighted-character))]
    (fn []
      (let [highlighted-character (:highlighted-character @state-atom)]
        (gui/black-background (layouts/with-margin 50
                                (layouts/horizontally-2 {:margin 10}
                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/finnish-statistics-without-å]
                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave (:fi key-log/statistics-from-key-log)]
                                                        [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/wikinews-finnish-statistics]

                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/english-statistics]
                                                        [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/keyboard-design-com-english-text-statistics]

                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/wikibooks-english-statistics]
                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/wikinews-english-statistics]
                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave (:en key-log/statistics-from-key-log)]

                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave (:hybrid key-log/statistics-from-key-log)]
                                                        ;; [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/hybrid-statistics-without-å]
                                                        [text-statistics-view highlighted-character on-mouse-enter on-mouse-leave text/wikinews-hybrid-statistics]
                                                        )))))))

(comment
  (view/start-view #'optimized-layouts-comparison-view)
  (view/start-view #'optimization-status-with-rating-comparison-view)
  (view/start-view #'optimized-layouts-comparison-view-2 {:join? true})
  (view/start-view #'optimized-layouts-comparison-view-2)
  ;; hot-right-now TODO: remove me
  (view/start-view (fn [] (gui/black-background [keyboard-view/keyboard-view (layout/layout-to-cocoa-key-code-to-character @edited-layout-atom)])))

  (view/start-view #'text-statistics-comparison-view)

  (def hill-climbed-layout (assoc (update (last (best-layouts-per-statistics-and-multipliers-with-names 1 0))
                                          :layout (fn [layout]
                                                    (optimize/hill-climb-all text/keyboard-design-com-english-text-statistics
                                                                             {:digram-roll 1,
                                                                              :trigram-roll 1,

                                                                              :horizontal-movement 1,
                                                                              :vertical-movement 1,
                                                                              :vertical-movement-in-skipgram 1

                                                                              :key-rating 1,
                                                                              :finger-type 1,

                                                                              :hand-balance 1
                                                                              :dist-from-colemak 0}
                                                                             #_{:digram-roll 1,
                                                                                :trigram-roll 1,

                                                                                :horizontal-movement 1,
                                                                                :vertical-movement 1,
                                                                                :vertical-movement-in-skipgram 1

                                                                                :key-rating 0.5,
                                                                                :finger-type 0.5,

                                                                                :hand-balance 0.1
                                                                                :dist-from-colemak 0.1}
                                                                             layout)))
                                  :name "hill-climbed-3"))

  (def last-layout (assoc (last (best-layouts-per-statistics-and-multipliers-with-names 1 0))
                          :name "last"))

  (map (fn [named-layout]
         (update named-layout :layout (partial optimize/hill-climb-all text/keyboard-design-com-english-text-statistics)))


       (map :multipliers (best-layouts-per-statistics-and-multipliers-with-names 1 0))
       )

  )


#_(view/refresh-view!)
(view/hard-refresh-view!)
