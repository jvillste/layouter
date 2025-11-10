(ns layouter.core
  (:require
   [clj-async-profiler.core :as clj-async-profiler]
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [com.stuartsierra.frequencies :as frequencies]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.path :as path]
   [flow-gl.gui.visuals :as visuals]
   [flow-gl.gui.window :as window]
   [fungl.application :as application]
   [fungl.color :as color]
   [fungl.component.button :as button]
   [fungl.component.text-area :as text-area]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [layouter.excercise :as excercise]
   [logga.core :as logga]
   [medley.core :as medley]
   [medley.core :as meldey])
  (:import
   (java.util Random)
   java.util.Locale))

(defonce optimized-layouts-atom (atom []))


;; KEYLOGGGER LOG PARSING


(defn parse-key-log [key-log]
  (->> (string/split key-log #",")
       (remove empty?)
       (map string/trim)
       (map (fn [line]
              (map parse-long (string/split line #" "))))
       (map (fn [[keycode time]]
              {:keycode keycode
               :time time}))))

(def key-log-file-path "/Users/jukka/nitor-src/posti/matching/temp/data/keylog.txt")

(def space-cocoa-key-code 49)
(def back-space-cocoa-key-code 51)
(def second-in-microseconds 1000000)

(defn key-log-to-string [parsed-log minimum-pause-for-inserting-space]
  (->> parsed-log
       ;; (sort-by :time)
       (partition-all 2 1)
       (mapcat (fn [[event following-event]]
                 (if (and following-event
                          (<= minimum-pause-for-inserting-space
                              (- (:time following-event)
                                 (:time event))))
                   [event
                    {:keycode space-cocoa-key-code
                     :time (inc (:time event))}]
                   [event])))
       (map :keycode)
       (map (assoc (layout-to-cocoa-key-code-to-character qwerty)
                   space-cocoa-key-code " "
                   back-space-cocoa-key-code " "))
       (string/join "")))

(deftest test-key-log-to-string
  (is (= "ab c"
         (key-log-to-string (let [character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code qwerty)]
                              [{:keycode (character-to-cocoa-key-code "a"), :time 0}
                               {:keycode (character-to-cocoa-key-code "b"), :time 1}
                               {:keycode (character-to-cocoa-key-code "c"), :time 3}])
                            2))))

(defn key-log-to-string-from-file [file-name]
  (-> file-name
      (slurp)
      (parse-key-log)
      (key-log-to-string (* 2 second-in-microseconds))))

;; TODO: keylogger logs characters that were not typed by me
;; keylogger should also log characters rather than keycodes

(comment
  (string/join  " " (extract-words (key-log-to-string-from-file "/Users/jukka/nitor-src/posti/matching/temp/data/keylog_kCGHIDEventTap.txt")))
  (key-log-to-string (parse-key-log (slurp key-log-file-path)) second-in-microseconds)

  (->> (parse-key-log (slurp key-log-file-path))
       (map :keycode)
       (map (layout-to-cocoa-key-code-to-character qwerty))
       (string/join ""))

  (let [key-pair-means (->> (parse-key-log (slurp key-log-file-path))
                            (filter :down?)
                            (partition 2 1)
                            (map (fn [[a b]]
                                   {:from (key-code-to-character (:keycode a))
                                    :to   (key-code-to-character (:keycode b))
                                    :time (- (:time b)
                                             (:time a))}))
                            (remove (fn [key-pair]
                                      (or (= " " (:from key-pair))
                                          (= " " (:to key-pair))
                                          (= nil (:from key-pair))
                                          (= nil (:to key-pair))
                                          (< 1000000 (:time key-pair))
                                          (= (:from key-pair)
                                             (:to key-pair)))))
                            (group-by (juxt :from :to))
                            (medley/map-vals (fn [key-pairs]
                                               (frequencies/mean (frequencies (map :time key-pairs))))))
        mean           (frequencies/mean (frequencies (vals key-pair-means)))]

    (->> (medley/map-vals (fn [value]
                            (float (/ value mean)))
                          key-pair-means)
         (sort-by second)))

  )



(def target-text "hello world")

(def ga-and-gd-optimized-hybrid-layout #{{:cocoa-key-code 2, :character "a"}
                                         {:cocoa-key-code 1, :character "s"}
                                         {:cocoa-key-code 33, :character "x"}
                                         {:cocoa-key-code 8, :character "l"}
                                         {:cocoa-key-code 46, :character "ä"}
                                         {:cocoa-key-code 45, :character "p"}
                                         {:cocoa-key-code 7, :character "u"}
                                         {:cocoa-key-code 38, :character "e"}
                                         {:cocoa-key-code 12, :character "z"}
                                         {:cocoa-key-code 4, :character "o"}
                                         {:cocoa-key-code 35, :character "å"}
                                         {:cocoa-key-code 17, :character "v"}
                                         {:cocoa-key-code 14, :character "c"}
                                         {:cocoa-key-code 44, :character "q"}
                                         {:cocoa-key-code 0, :character "h"}
                                         {:cocoa-key-code 50, :character ""}
                                         {:cocoa-key-code 40, :character "n"}
                                         {:cocoa-key-code 43, :character "m"}
                                         {:cocoa-key-code 5, :character "k"}
                                         {:cocoa-key-code 47, :character "y"}
                                         {:cocoa-key-code 41, :character "d"}
                                         {:cocoa-key-code 34, :character "r"}
                                         {:cocoa-key-code 9, :character "j"}
                                         {:cocoa-key-code 3, :character "t"}
                                         {:cocoa-key-code 31, :character "ö"}
                                         {:cocoa-key-code 39, :character "w"}
                                         {:cocoa-key-code 32, :character "g"}
                                         {:cocoa-key-code 6, :character "b"}
                                         {:cocoa-key-code 37, :character "i"}
                                         {:cocoa-key-code 15, :character "f"}})





(defn csv-rows-to-string [rows]
  (string/join "\n"
               (for [row rows]
                 (string/join ";" row))))

(deftest test-csv-rows-to-string
  (is (= ":a;:b\n:c;:d"
         (csv-rows-to-string [[:a :b]
                              [:c :d]]))))

(defn format-rows [column-width rows]
  (string/join "\n"
               (for [row rows]
                 (apply format
                        (apply str
                               (repeat (count row)
                                       (str "%" column-width "s")))
                        row))))

(defn print-rows [column-width rows]
  (println (format-rows column-width rows)))

(comment
  (print-rows 30
              [[:a :b]
               [:c :d]])
  )




(defn sort-text-statistics [text-statistics]
  [(reverse (sort-by second (:character-distribution text-statistics)))
   (reverse (sort-by second (:digram-distribution text-statistics)))])


(defn rating-description-to-text [text-statistics layout]
  (let [rating-description (describe-layout-rating text-statistics
                                                   layout)]
    (str (with-out-str (pprint/pprint (summarize-rating-description rating-description)))
         "\n"
         (format-rows 31
                      (rating-description-to-rows rating-description)))))

(defn comparison-table [data]
  (for [column (into #{} (apply concat (map keys (vals data))))]
    {:title column
     :min (apply min (map column (vals data)))
     :max (apply max (map column (vals data)))}))

(deftest test-comparison-table
  (is (= ({:title :m2, :min 2, :max 3}
          {:title :m1, :min 1, :max 2})
         (comparison-table {:a {:m1 1
                                :m2 2}
                            :b {:m1 2
                                :m2 3}}))))

(defn optimize-with-multipliers [multipliers text-statistics & [{:keys [initial-layout]}]]
  (binding [multipliers multipliers]
    (let [layout (optimize text-statistics initial-layout)
          #_(gradient-descent-all text-statistics
                                  #_(random-layout (keys (:character-distribution text-statistics)))
                                  (optimize text-statistics initial-layout))]
      ;; (println (rate-layout text-statistics
      ;;                       layout))
      layout)))


(defn optimize-named-layout-with-multipliers [multipliers statistics & [{:keys [initial-layout]}]]
  {:multipliers multipliers
   :statistics-name (:name statistics)
   :layout (optimize-with-multipliers multipliers
                                      statistics
                                      {:initial-layout initial-layout})})

(comment
  (def optimized-test-layout (optimize-with-multipliers multipliers hybrid-statistics))
  (start-view (fn []
                (black-background [keyboard-view
                                   (layout-to-cocoa-key-code-to-character optimized-test-layout)
                                   key-colors-for-fingers])))

  (set/difference (set (map :qwerty-character keyboard-keys))
                  (set (map :character optimized-test-layout)))

  )




(defonce optimized-layout qwerty)

(defn distribution-positions [distribution]
  (->> distribution
       (sort-by second)
       (reverse)
       (map first)
       (map-indexed vector)
       (map reverse)
       (map vec)
       (into {})))

(defn remove-spaces [text]
  (string/replace text #"\s+" ""))

(defn normalized-character-distribution [text characters]
  (->> (filter-target-text-without-space text characters)
       frequencies
       normalize-distribution
       (medley/map-keys str)))

(deftest test-normalized-character-distribution
  (is (= {"h" 0.125, "e" 0.125, "l" 0.25, "o" 0.5}
         (normalized-character-distribution "hello ooo" ["h" "e" "l" "o"]))))


(defn compare-distributions [distributions]
  (doseq [value (reverse (sort-by (apply merge (reverse distributions))
                                  (set (mapcat keys distributions))))]
    (let [proportional-differences (map #(/ (or (% value)
                                                0)
                                            (or ((first distributions) value)
                                                0.000001))
                                        (rest distributions))]
      (when (< 0.5
               (abs (- (first proportional-differences)
                       1)))
        (apply println
               (pr-str value)
               (concat (map #(% value)
                            distributions)
                       ))))))

(defn compare-character-cistributions []
  (compare-distributions [(normalized-character-distribution (str (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                                  (slurp "temp/text/the-hacker-crackdown.txt")))
                          (normalized-character-distribution (key-log-to-string key-log-file-path))]))

(defn compare-digram-distributions []
  (compare-distributions [ ;; (normalized-digram-distribution (slurp "temp/text/the-hacker-crackdown.txt"))
                          ;; (normalized-digram-distribution (slurp "temp/text/tietokoneet.txt"))
                          ;; (normalized-digram-distribution (slurp "temp/text/the-hacker-crackdown.txt"))
                          ;; (normalized-digram-distribution (remove-spaces (slurp "temp/text/the-hacker-crackdown.txt")))
                          (normalized-digram-distribution (str (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                               (slurp "temp/text/the-hacker-crackdown.txt")))
                          (normalized-digram-distribution (key-log-to-string key-log-file-path))]))

(comment
  (compare-distributions [(normalized-character-distribution "abccc")
                          (normalized-character-distribution "abdd")])
  (compare-character-cistributions)
  (compare-digram-distributions)
  (normalized-character-distribution "hello")
  (def english-digram-distribution (normalized-digram-distribution (slurp "temp/text/the-hacker-crackdown.txt")))

  (def hacker-distribution (normalized-digram-distribution (slurp "temp/text/the-hacker-crackdown.txt")))
  (def corncob-distribution (normalized-digram-distribution (slurp "temp/text/corncob_lowercase.txt")))

  ;; (let [first-distribution hacker-distribution
  ;;       second-distribution (normalized-digram-distribution (slurp "temp/text/ga.txt"))

  ;;       first-positions (distribution-positions first-distribution)
  ;;       second-positions (distribution-positions second-distribution)

  ;;       digram-distributions [(normalized-digram-distribution (slurp "temp/text/the-hacker-crackdown.txt"))
  ;;                             (normalized-digram-distribution (slurp "temp/text/tietokoneet.txt"))]
  ;;       ]

  ;;   (doseq [digram (sort-by (apply merge (map distribution-positions (reverse digram-distributions)))
  ;;                           (apply set/union
  ;;                                  (map set
  ;;                                       (map #(map first %)
  ;;                                            digram-distributions))))

  ;;           ;; (sort-by (merge second-positions
  ;;           ;;                 first-positions)
  ;;           ;;          (set/union (set (map first first-distribution))
  ;;           ;;                     (set (map first second-distribution))))
  ;;           ]
  ;;     (apply println
  ;;            digram
  ;;            (first-positions digram)
  ;;            (second-positions digram))))





  (count (normalized-digram-distribution (slurp "temp/text/corncob_lowercase.txt")))
  ;; => 342
  (count english-digram-distribution)

  (def optimized-layout qwerty)
  (optimize (filter-target-text target-text)
            optimized-layout
            100
            (fn [new-layout]
              (def optimized-layout new-layout)))

  (count (normalized-digram-distribution (filter-target-text (slurp "temp/text/kirjoja-ja-kirjailijoita.txt"))))
  ;; => 413

  (doseq [layout-file-name ["qwerty.edn"
                            "optimized-layout-for-finnish-and-english.edn"
                            "optimized-layout-for-finnish.edn"
                            "optimized-layout-for-english.edn"]]
    (println)
    (println layout-file-name)
    (let [layout (edn/read-string (slurp layout-file-name))]
      (doseq [target-text-file-name ["temp/text/the-hacker-crackdown.txt"
                                     "temp/text/kirjoja-ja-kirjailijoita.txt"]]
        (println target-text-file-name
                 (rate-layout (take 300 (reverse (sort-by second (normalized-digram-distribution (slurp target-text-file-name)))))
                              layout)))))

  ;; qwerty.edn
  ;; temp/text/the-hacker-crackdown.txt -8.200177457130849
  ;; temp/text/kirjoja-ja-kirjailijoita.txt -8.067552298454757

  ;; optimized-layout-for-finnish-and-english.edn
  ;; temp/text/the-hacker-crackdown.txt -5.41650933789515
  ;; temp/text/kirjoja-ja-kirjailijoita.txt -4.781927577581428

  ;; optimized-layout-for-finnish.edn
  ;; temp/text/the-hacker-crackdown.txt -5.869223441281065
  ;; temp/text/kirjoja-ja-kirjailijoita.txt -4.6758328653527315

  ;; optimized-layout-for-english.edn
  ;; temp/text/the-hacker-crackdown.txt -5.3888708559434235
  ;; temp/text/kirjoja-ja-kirjailijoita.txt -5.800231798034474

  )



;; UI






(comment
  (:column (cocoa-key-code-to-key ((layout-to-character-to-cocoa-key-code qwerty) "s")))
  ) ;; TODO: remove me




(defn character-view [character-to-cocoa-key-code on-mouse-over-character [character next-character]]
  (let [character-key (cocoa-key-code-to-key (character-to-cocoa-key-code character))
        next-character-key (cocoa-key-code-to-key (character-to-cocoa-key-code next-character))]
    {:node (layouts/vertically-2 {}
                                 (text character)
                                 (text (or (:finger character-key)
                                           "_")
                                       {:color (if (and (not (= character-key
                                                                next-character-key))
                                                        (= (:finger character-key)
                                                           (:finger next-character-key)))
                                                 [200 100 100 255]
                                                 (:text-color theme))})
                                 (text (or (:row character-key)
                                           " "))
                                 (text (or (finger-hand (:finger character-key))
                                           " ")
                                       {:color (if (and (not (= character-key
                                                                next-character-key))
                                                        (= (finger-hand (:finger character-key))
                                                           (finger-hand (:finger next-character-key))))
                                                 [200 100 100 255]
                                                 (:text-color theme))})
                                 (let [key-rating (when (contains? layout-characters character)
                                                    (abs (int (total-effort (rate-key character-key)))))]
                                   (text key-rating
                                         {:color (if (not (or (= 0 key-rating)
                                                              (= 1 key-rating)))
                                                   [200 100 100 255]
                                                   (:text-color theme))}))
                                 (text (when (and (contains? layout-characters character)
                                                  (contains? layout-characters next-character))
                                         (str (abs (int (/ (total-effort (rate-key-pair [(cocoa-key-code-to-key (character-to-cocoa-key-code character))
                                                                                         (cocoa-key-code-to-key (character-to-cocoa-key-code next-character))]))
                                                           2)))))))
     :mouse-event-handler (fn [node event]
                            (when (= :nodes-under-mouse-changed (:type event))
                              (if (contains? (set (map :id (:nodes-under-mouse event)))
                                             (:id node))
                                (on-mouse-over-character character)
                                (on-mouse-over-character nil)))
                            event)}))

(defn digram-view [digram-distribution character-to-cocoa-key-code on-mouse-over-digram]
  (layouts/vertically-2 {}
                        (for [digram (take 30 (reverse (sort-by second digram-distribution)))]
                          {:node (text (str (apply str (first digram))
                                            " "
                                            (rate-key-pair (map (fn [character]
                                                                  (cocoa-key-code-to-key (character-to-cocoa-key-code character)))
                                                                (first digram))
                                                           #_[(cocoa-key-code-to-key (character-to-cocoa-key-code (first (first digram))))
                                                              (cocoa-key-code-to-key (character-to-cocoa-key-code (second (first digram))))])))
                           :mouse-event-handler (fn [node event]
                                                  (when (= :nodes-under-mouse-changed (:type event))
                                                    (if (= (:id node)
                                                           (:id (last (:nodes-under-mouse event))))
                                                      (on-mouse-over-digram (first digram))
                                                      (on-mouse-over-digram nil)))

                                                  (when (= :mouse-left (:type event))
                                                    (on-mouse-over-digram nil))
                                                  event)})))
(comment
  (format "%.2f" 0.1234)
  ) ;; TODO: remove me
(comment
  (let [cocoa-key-code-to-column (merge
                                  (into {} (map vec (map reverse (map-indexed vector [12 13 14 15 17 16 32 34 31 35 33]))))
                                  (into {} (map vec (map reverse (map-indexed vector [0 1 2 3 5 4 38 40 37 41 39]))))
                                  (into {} (map vec (map reverse (map-indexed vector [6 7 8 9 11 45 46 43 47 44])))))]
    (for [keyboard-key keyboard-keys]
      (assoc keyboard-key
             :column (cocoa-key-code-to-column (:cocoa-key-code keyboard-key)))))



  ) ;; TODO: remove me

(comment
  (vec (map (fn [color]
              (conj (vec (map (comp int
                                    (partial * 0.7))
                              (take 3 color)))
                    255))
            [[150 70 100 255]
             [100 70 150 255]
             [150 150 70 255]
             [100 150 70 255]]))
  ) ;; TODO: remove me



(comment
  (start-view (fn []
                [keyboard-view (into {} (for [keyboard-key keyboard-keys]
                                          [(:cocoa-key-code keyboard-key)
                                           (str (:column keyboard-key))]))
                 (constantly [128 128 128 255])]))

  (start-view (fn []
                (black-background [keyboard-view (into {} (for [keyboard-key keyboard-keys]
                                                            [(:cocoa-key-code keyboard-key)
                                                             (key-class-effort (:class keyboard-key))]))
                                   (fn [cocoa-key-code]
                                     (let [shade (float (key-class-effort (:class (cocoa-key-code-to-key cocoa-key-code))))]
                                       [shade shade shade 1.0]))])))

  (start-view (fn []
                (layouts/with-minimum-size 100 100 (box (text "x")
                                                        {:fill-color [200 0 0 255]
                                                         :padding 5}))))

  (start-view (fn []
                (layouts/superimpose
                 (layouts/with-minimum-size 100 100
                   (visuals/rectangle-2 :fill-color [200 0 0 255])))))


  ) ;; TODO: remove me


(defn key-colors-for-key-ratings []
  (into {}
        (for [keyboard-key keyboard-keys]
          [(:cocoa-key-code keyboard-key)
           (let [shade (+ 10
                          (* 246
                             (or (/ (total-effort (rate-key keyboard-key))
                                    2)
                                 0)))]
             [shade shade shade 255])])))


(comment
  (start-view (fn []
                [#'layout-editor
                 (dependable-atom/atom qwerty)
                 hybrid-statistics]))
  ) ;; TODO: remove me

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
        cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character (:layout layout))
        character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code (:layout layout))
        rating-to-aspect (into {} (for [aspect (keys distribution-rating-description)
                                        rating (map :rating (:ratings (first (get distribution-rating-description aspect))))]
                                    [rating aspect]))]
    (fn [_layout rating distribution-rating-description]
      (let [key-color (into {}
                            (concat (for [highlighted-character (:highlighted-characters @state-atom)]
                                      [(character-to-cocoa-key-code highlighted-character) [100 150 70 255]])))]
        (layouts/vertically-2 {:margin 10}
                              ;; (text (:name layout))
                              (layout-comparison-text (pr-str (:multipliers layout)))
                              [keyboard-view cocoa-key-code-to-character
                               (merge key-colors-for-fingers
                                      key-color)]
                              [rating-description-table state-atom rating-to-aspect distribution-rating-description rating])))))

(comment
  (start-view (fn [] [distribtion-rating-description-view {:name :random
                                                           :layout (random-layout)}
                      :vertical-movement
                      '{:digrams
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
                           {:rating :key, :label :home, :effort 0}]})}]))

  ) ;; TODO: remove me

(defn on-click [handler node]
  {:node node
   :mouse-event-handler (fn [_node event]
                          (when (= :mouse-clicked (:type event))
                            (handler))
                          event)})

(defn multiply-color [multiplier color]
  (vec (concat (map (partial * multiplier)
                    (take 3 color))
               [(last color)])))

(deftest test-multiply-color
  (is (= [2 2 2 1]
         (multiply-color 2 [1 1 1 1]))))


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




(def test-events '({:y 516, :shift false, :key :udefied, :alt false, :time 1715012242345, :type :mouse-moved, :source :mouse, :cotrol false, :x 604} {:y 518, :shift false, :key :udefied, :alt false, :time 1715012242352, :type :mouse-moved, :source :mouse, :cotrol false, :x 608} {:y 518, :shift false, :key :udefied, :alt false, :time 1715012242353, :type :mouse-moved, :source :mouse, :cotrol false, :x 608} {:y 520, :shift false, :key :udefied, :alt false, :time 1715012242360, :type :mouse-moved, :source :mouse, :cotrol false, :x 614} {:y 520, :shift false, :key :udefied, :alt false, :time 1715012242361, :type :mouse-moved, :source :mouse, :cotrol false, :x 614} {:y 520, :shift false, :key :udefied, :alt false, :time 1715012242369, :type :mouse-moved, :source :mouse, :cotrol false, :x 618} {:y 520, :shift false, :key :udefied, :alt false, :time 1715012242369, :type :mouse-moved, :source :mouse, :cotrol false, :x 618} {:y 522, :shift false, :key :udefied, :alt false, :time 1715012242377, :type :mouse-moved, :source :mouse, :cotrol false, :x 624} {:y 522, :shift false, :key :udefied, :alt false, :time 1715012242377, :type :mouse-moved, :source :mouse, :cotrol false, :x 624} {:y 524, :shift false, :key :udefied, :alt false, :time 1715012242385, :type :mouse-moved, :source :mouse, :cotrol false, :x 628} {:y 524, :shift false, :key :udefied, :alt false, :time 1715012242385, :type :mouse-moved, :source :mouse, :cotrol false, :x 628} {:y 526, :shift false, :key :udefied, :alt false, :time 1715012242393, :type :mouse-moved, :source :mouse, :cotrol false, :x 634} {:y 526, :shift false, :key :udefied, :alt false, :time 1715012242394, :type :mouse-moved, :source :mouse, :cotrol false, :x 634} {:y 526, :shift false, :key :udefied, :alt false, :time 1715012242401, :type :mouse-moved, :source :mouse, :cotrol false, :x 638} {:y 526, :shift false, :key :udefied, :alt false, :time 1715012242402, :type :mouse-moved, :source :mouse, :cotrol false, :x 638} {:y 528, :shift false, :key :udefied, :alt false, :time 1715012242409, :type :mouse-moved, :source :mouse, :cotrol false, :x 646} {:y 528, :shift false, :key :udefied, :alt false, :time 1715012242410, :type :mouse-moved, :source :mouse, :cotrol false, :x 646} {:y 528, :shift false, :key :udefied, :alt false, :time 1715012242417, :type :mouse-moved, :source :mouse, :cotrol false, :x 650} {:y 528, :shift false, :key :udefied, :alt false, :time 1715012242418, :type :mouse-moved, :source :mouse, :cotrol false, :x 650} {:y 530, :shift false, :key :udefied, :alt false, :time 1715012242425, :type :mouse-moved, :source :mouse, :cotrol false, :x 654}))

(defn random-mouse-move-event []
  {:y (rand-int 1000), :shift false, :key :udefied, :alt false, :time (System/currentTimeMillis), :type :mouse-moved, :source :mouse, :cotrol false, :x (rand-int 1000)})

(defn interval [framerate]
  (let [millisecond 1000000]
    (/ (* 1000 millisecond)
       framerate)))



(def stack "java/lang/Thread.run;java/lang/Thread.runWith;clojure/lang/AFn.run;nrepl/middleware/session$session_exec$main_loop__20298.invoke;nrepl/middleware/session$session_exec$main_loop__20298$fn__20302.invoke;clojure/lang/AFn.run;nrepl/middleware/interruptible_eval$interruptible_eval$fn__20228$fn__20232.invoke;nrepl/middleware/interruptible_eval$evaluate.invoke;nrepl/middleware/interruptible_eval$evaluate.invokeStatic;clojure/lang/RestFn.invoke;clojure/main$repl.doInvoke;clojure/main$repl.invokeStatic;clojure/main$repl$fn__9215.invoke;clojure/main$repl$read_eval_print__9206.invoke;clojure/main$repl$read_eval_print__9206$fn__9209.invoke;nrepl/middleware/interruptible_eval$evaluate$fn__20195.invoke;clojure/lang/RestFn.invoke;clojure/core$with_bindings_STAR_.doInvoke;clojure/core$with_bindings_STAR_.invokeStatic;clojure/core$apply.invokeStatic;clojure/lang/AFn.applyTo;clojure/lang/AFn.applyToHelper;nrepl/middleware/interruptible_eval$evaluate$fn__20195$fn__20196.invoke;clojure/core$eval.invoke;clojure/core$eval.invokeStatic;clojure/lang/Compiler.eval;clojure/lang/Compiler.eval;layouter/core$eval57607.invoke;layouter/core$eval57607.invokeStatic;clojure/lang/RestFn.invoke;clojure/core$with_bindings_STAR_.doInvoke;clojure/core$with_bindings_STAR_.invokeStatic;clojure/core$apply.invokeStatic;clojure/lang/AFn.applyTo;clojure/lang/AFn.applyToHelper;layouter/core$eval57607$fn__57610.invoke;layouter/core$eval57607$fn__57610$fn__57615.invoke;fungl/application$application_loop_render_BANG_.invoke;fungl/application$application_loop_render_BANG_.invokeStatic;flow_gl/swing/window/SwingWindow.run_with_gl;fungl/application$application_loop_render_BANG_$fn__50302.invoke;fungl/application$render.invoke;fungl/application$render.invokeStatic;fungl/node_image_cache$render_recurring_nodes_to_images.invoke;fungl/node_image_cache$render_recurring_nodes_to_images.invokeStatic;flow_gl/gui/scene_graph$enumerate_nodes.invoke;flow_gl/gui/scene_graph$enumerate_nodes.invokeStatic;clojure/core$first__5449.invoke;clojure/core$first__5449.invokeStatic;clojure/lang/RT.first;clojure/lang/LazySeq.first;clojure/lang/LazySeq.seq;clojure/lang/LazySeq.sval;clojure/core$concat$fn__5558.invoke;clojure/core$seq__5467.invokeStatic;clojure/lang/RT.seq;clojure/lang/LazySeq.seq;clojure/lang/LazySeq.sval;clojure/core$concat$fn__5558.invoke;clojure/core$seq__5467.invokeStatic;clojure/lang/RT.seq;clojure/lang/LazySeq.seq;clojure/lang/LazySeq.sval;clojure/core$concat$fn__5558.invoke;clojure/core$seq__5467.invokeStatic;clojure/lang/RT.seq;clojure/lang/LazySeq.seq;clojure/lang/LazySeq.sval;clojure/core$concat$fn__5558.invoke;clojure/core$seq__5467.invokeStatic;clojure/lang/RT.seq;clojure/lang/LazySeq.seq;clojure/lang/LazySeq.sval;clojure/core$concat$fn__5558.invoke;clojure/core$seq__5467.invokeStatic;")

(defn optimized-layout-comparizon-view []
  [#'layout-comparison-view
   [(named-layout-to-named-layout-atom (first @optimized-layouts-atom))
    (named-layout-to-named-layout-atom (first @optimized-layouts-atom))]
   hybrid-statistics])

(defn toggle-pad-mouse-event-handler [state-atom _node event]
  (when (= :mouse-pressed (:type event))
    (swap! state-atom not))
  event)

(defn toggle-pad []
  (let [state-atom (dependable-atom/atom false)]
    (fn []
      (assoc (visuals/rectangle-2 {:fill-color (if @state-atom
                                                 [255 255 255 255]
                                                 [155 155 155 255])})
             :width 100
             :height 100
             :mouse-event-handler (fn [_node event]
                                    (when (= :mouse-pressed (:type event))
                                      (swap! state-atom not))
                                    event) #_[toggle-pad-mouse-event-handler state-atom]))))

(defn cache-test-view []
  (layouts/horizontally-2 {:margin 10}
                          (layouts/horizontally-2 {:margin 10}
                                                  (layouts/vertically-2 {:margin 10}
                                                                        [toggle-pad]
                                                                        [toggle-pad])
                                                  (layouts/vertically-2 {:margin 10}
                                                                        [toggle-pad]
                                                                        [toggle-pad]))
                          (layouts/horizontally-2 {:margin 10}
                                                  (layouts/vertically-2 {:margin 10}
                                                                        [toggle-pad]
                                                                        [toggle-pad])
                                                  (layouts/vertically-2 {:margin 10}
                                                                        [toggle-pad]
                                                                        [toggle-pad]))))

(defn cache-test-view-2 []
  (let [state-atom (dependable-atom/atom {:1 0
                                          :2 0})]
    (fn []
      (layouts/vertically-2 {:margin 10}
                            {:node [text "1"]
                             :mouse-event-handler (fn [_node event]
                                                    (when (= :mouse-pressed (:type event))
                                                      (swap! state-atom update :1 inc))
                                                    event)}
                            {:node [text "2"]
                             :mouse-event-handler (fn [_node event]
                                                    (when (= :mouse-pressed (:type event))
                                                      (swap! state-atom update :2 inc))
                                                    event)}
                            [text (:1 @state-atom)]
                            [text (:2 @state-atom)]))))


(def pad-1-toggle-event {:y 56, :shift false, :local-x 50, :key :unknown, :alt false, :time 1715350496806, :local-y 56, :type :mouse-pressed, :source :mouse, :control false, :x 50, :handling-phase :on-target})
(def pad-2-toggle-event {:y 180, :shift false, :local-x 50, :key :unknown, :alt false, :time 1715350498487, :local-y 70, :type :mouse-pressed, :source :mouse, :control false, :x 50, :handling-phase :on-target})
(def mouse-move-event {:y 61, :shift false, :cotrol false, :key :udefied, :alt false, :time 1715350606209, :type :mouse-moved, :source :mouse, :x 496})

(deftest test-scene-graph
  (with-bindings (application/create-bindings-without-window (fn []
                                                               (box )))

    (let [scene-graph (:scene-graph @application/application-loop-state-atom)]
      (application/handle-events! [mouse-move-event])
      (is (identical? scene-graph
                      (:scene-graph @application/application-loop-state-atom)))

      (println "---------- togling pad 1")
      (application/handle-events! [pad-1-toggle-event])

      (is (not (identical? scene-graph (:scene-graph @application/application-loop-state-atom))))

      (is (= (-> scene-graph :children second)
             (-> @application/application-loop-state-atom :scene-graph :children second)))

      (prn (-> scene-graph :children second :id)
           (System/identityHashCode (-> scene-graph :children second))
           (System/identityHashCode (-> @application/application-loop-state-atom :scene-graph :children second))) ;; TODO: remove me

      (is (identical? (-> scene-graph :children second)
                      (-> @application/application-loop-state-atom :scene-graph :children second))))

    (application/close-window! (:window @application/application-loop-state-atom))))

(deftest test-cache
  (println "\n\n------------ start test") ;; TODO: remove me

  (with-bindings (application/create-bindings-without-window #'cache-test-view)

    (let [scene-graph (:scene-graph @application/application-loop-state-atom)]
      (application/handle-events! [mouse-move-event])
      (is (identical? scene-graph
                      (:scene-graph @application/application-loop-state-atom)))

      (println "---------- togling pad 1")
      (application/handle-events! [pad-1-toggle-event])

      (is (not (identical? scene-graph (:scene-graph @application/application-loop-state-atom))))

      (is (= (-> scene-graph :children second)
             (-> @application/application-loop-state-atom :scene-graph :children second)))

      (prn (-> scene-graph :children second :id)
           (System/identityHashCode (-> scene-graph :children second))
           (System/identityHashCode (-> @application/application-loop-state-atom :scene-graph :children second))) ;; TODO: remove me

      (is (identical? (-> scene-graph :children second)
                      (-> @application/application-loop-state-atom :scene-graph :children second))))

    (application/close-window! (:window @application/application-loop-state-atom))))


(comment

  (start-view (fn []
                [#'cache-test-view-2]))

  (test-cache)

  )


(comment


  (string/split stack #";")


  (let [event-channel (application/start-application (fn []
                                                       [#'layout-comparison-view
                                                        [(named-layout-to-named-layout-atom (first @optimized-layouts-atom))
                                                         (named-layout-to-named-layout-atom (first @optimized-layouts-atom))]
                                                        hybrid-statistics]))]
    (doseq [event (take 1 @application/events-atom)]
      (async/>!! event-channel
                 event))

    (prn 'start-profiling) ;; TODO: remove me

    ;; (clj-async-profiler/profile {:interval 1000000}
    ;;               (doseq [event (take 1 @application/events-atom)]
    ;;                 (async/>!! event-channel
    ;;                            event)))

    (prn 'end-profiling) ;; TODO: remove me

    (doseq [event (take 1 @application/events-atom)]
      (async/>!! event-channel
                 event))

    (async/>!! event-channel
               {:type :close-requested}))

  (let [event-channel (application/start-application (fn []
                                                       [#'layout-comparison-view
                                                        [(named-layout-to-named-layout-atom (first @optimized-layouts-atom))
                                                         (named-layout-to-named-layout-atom (first @optimized-layouts-atom))]
                                                        hybrid-statistics]))]

    (doseq [event (take 10 @application/events-atom)]
      (logga/write 'sending-event event) ;; TODO: remove me

      (async/>!! event-channel
                 event))
    ;;    (Thread/sleep 1000)

    (async/>!! event-channel
               {:type :close-requested}))

  (clj-async-profiler/serve-ui 9898)

  (count test-events)
  (with-bindings (application/create-bindings (fn []
                                                [#'layout-comparison-view
                                                 [(named-layout-to-named-layout-atom (first @optimized-layouts-atom))
                                                  (named-layout-to-named-layout-atom (first @optimized-layouts-atom))]
                                                 hybrid-statistics]))

    (application/handle-events! #_[{:type :resize-requested, :width 3780.0, :height 3278.0}]
                                (application/read-events (window/event-channel (:window @application/application-loop-state-atom))
                                                         60)
                                ;;(take 1 @application/events-atom)
                                )

    ;; (application/application-loop-render!)

    (doseq [event (repeatedly 1 random-mouse-move-event) #_(take 100 test-events)]
      (application/handle-events! [event])
      (application/application-loop-render!))

    (logga/write "start profiling")
    (clj-async-profiler/profile {:event :cpu #_:alloc
                                 :interval (interval 1000)}
                                (doseq [event (repeatedly 50 random-mouse-move-event) #_(take 2 test-events)]
                                  (application/handle-events! [event])
                                  (application/application-loop-render!))

                                ;; (application/handle-events! (take 10 @application/events-atom))
                                ;; (application/application-loop-render!)
                                )
    (logga/write "end profiling")

    ;; (application/handle-events! (take 100 @application/events-atom))
    ;; (application/application-loop-render!)
    (application/close-window! (:window @application/application-loop-state-atom))
    )
  )

(defn add-optimized-layout [multipliers statistics]
  (swap! optimized-layouts-atom
         conj
         (optimize-named-layout-with-multipliers multipliers
                                                 statistics))
  (refresh-view!))

(def common-multipliers
  {:digram-roll 1, :trigram-roll 1, :key-rating 1, :finger-type 1, :horizontal-movement 1, :vertical-movement 1, :hand-balance 1}
  #_{:digram-roll 0.5, :trigram-roll 0.0, :key-rating 1, :finger-type 0.2, :horizontal-movement 0.1, :vertical-movement 0.1, :hand-balance 0.1})


(defonce chosen-layout {:layout qwerty})

(defn layout-excercise-view []
  (let [layout (:layout chosen-layout)
        statistics hybrid-statistics
        cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)
        character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)
        state-atom (dependable-atom/atom {:character-count 4
                                          :typed-text ""
                                          :cocoa-key-code-down nil
                                          :target-word (excercise/excericse-word 4 statistics)})]
    (fn []
      (let [characters (excercise/take-most-common-characters (:character-count @state-atom)
                                                              (:character-distribution statistics))]
        {:node (black-background (layouts/center (layouts/vertically-2 {:margin 10 :centered? true}
                                                                       (text (:target-word @state-atom))
                                                                       (text (:typed-text @state-atom))
                                                                       (keyboard-view cocoa-key-code-to-character
                                                                                      (merge key-colors-for-fingers
                                                                                             (-> (into {}
                                                                                                       (for [character (drop-last characters)]
                                                                                                         [(character-to-cocoa-key-code character)
                                                                                                          [0 0 0 255]]))
                                                                                                 (assoc (character-to-cocoa-key-code (last characters))
                                                                                                        [0.5 0 0 255])
                                                                                                 (assoc (:cocoa-key-code-down @state-atom)
                                                                                                        [0 0.8 0 255]))))
                                                                       (text (str (:character-count @state-atom)
                                                                                  " / "
                                                                                  (last characters))))))
         :can-gain-focus? true
         :keyboard-event-handler (fn [_subtree event]
                                   (when (= :key-released
                                            (:type event))
                                     (swap! state-atom dissoc :cocoa-key-code-down))

                                   (when (= :key-pressed
                                            (:type event))

                                     (swap! state-atom assoc :cocoa-key-code-down (java-key-code-to-cocoa-key-code (:key-code event) ))

                                     (when (= :back-space (:key event))
                                       (swap! state-atom update :typed-text (fn [typed-text]
                                                                              (string/join (drop-last typed-text)))))

                                     (when (and (= :up (:key event))
                                                (> (count layout)
                                                   (:character-count @state-atom)))
                                       (swap! state-atom (fn [state]
                                                           (-> state
                                                               (update :character-count inc)
                                                               (assoc :typed-text ""
                                                                      :target-word (excercise/excericse-word (inc (:character-count @state-atom))
                                                                                                             statistics))))))
                                     (when (and (= :down (:key event))
                                                (< 2 (:character-count @state-atom)))
                                       (swap! state-atom (fn [state]
                                                           (-> state
                                                               (update :character-count dec)
                                                               (assoc :typed-text ""
                                                                      :target-word (excercise/excericse-word (inc (:character-count @state-atom))
                                                                                                             statistics))))))

                                     (when-some [character (cocoa-key-code-to-character (java-key-code-to-cocoa-key-code (:key-code event) ))]
                                       (swap! state-atom update :typed-text str character))

                                     (when (= (:target-word @state-atom)
                                              (:typed-text @state-atom))
                                       (swap! state-atom (fn [state]
                                                           (assoc state
                                                                  :typed-text ""
                                                                  :target-word (excercise/excericse-word (:character-count @state-atom)
                                                                                                         statistics)))))))}))))


(comment
  (start-view #'layout-excercise-view)

  (reset! optimized-layouts-atom [])


  (map layout-name @optimized-layouts-atom)
  (def chosen-layout (last @optimized-layouts-atom))
  (def chosen-layout (read-string (slurp "temp/chosen-layout.edn")))
  (spit "temp/chosen-layout.edn"
        (pr-str chosen-layout))

  (binding [generation-size 100
            number-of-genrations 100]
    (add-optimized-layout {:digram-roll 1
                           :trigram-roll 0
                           :key-rating 1
                           :finger-type 0.1
                           :horizontal-movement 0.1
                           :vertical-movement 1
                           :hand-balance 0.1}
                          hybrid-statistics
                          #_english-statistics-with-diacritics
                          #_finnish-statistics))

  (optimize-named-layout-with-multipliers multipliers
                                          english-statistics)

  (start-view (fn []
                [#'layout-comparison-view
                 (->> @ratings-atom
                      (sort-by second)
                      (map (fn [[layout _rating]]
                             {:layout layout
                              :name "optimized"}))
                      (map named-layout-to-named-layout-atom)
                      (take 20))
                 hybrid-statistics]))


  (do (def named-layout-atom-1 (named-layout-to-named-layout-atom {:layout (->> @ratings-atom
                                                                                (sort-by second)
                                                                                (first)
                                                                                (first))
                                                                   :name "optimized 0"})

        #_(assoc (named-layout-to-named-layout-atom {:layout colemak-dh})
                 :name "coleman-dh")
        #_(assoc (named-layout-to-named-layout-atom {:layout qwerty})
                 :name "qwerty")
        #_(named-layout-to-named-layout-atom (first @optimized-layouts-atom)))
      (def named-layout-atom-2 (->> @ratings-atom
                                    (sort-by second)
                                    (second)
                                    (map (fn [[layout rating]]
                                           {:layout layout
                                            :name "optimized"}))
                                    (map named-layout-to-named-layout-atom))

        #_(named-layout-to-named-layout-atom (first @optimized-layouts-atom)))
      (start-view (fn []
                    [#'layout-comparison-view
                     #_[named-layout-atom-1
                        named-layout-atom-2]
                     #_hybrid-statistics
                     #_finnish-statistics

                     (concat [(named-layout-to-named-layout-atom {:name "ga-and-gd-optimized"
                                                                  :layout ga-and-gd-optimized-hybrid-layout})]
                             (->> @optimization-history-atom
                                  (last)
                                  (:ratings)
                                  (sort-by second)
                                  (map (fn [[layout _rating]]
                                         {:layout layout
                                          :name "optimized"}))
                                  (map named-layout-to-named-layout-atom)
                                  (take 2)))
                     hybrid-statistics]))
      )

  (clojure.data/diff @(:layout-atom named-layout-atom-1)
                     @(:layout-atom named-layout-atom-2))

  (->> (describe-layout-rating hybrid-statistics
                               @(:layout-atom named-layout-atom-2))
       :distributions
       :digrams
       (filter (fn [digram]
                 (= :same-finger-one-row-leap
                    (:label (meldey/find-first (fn [rating]
                                                 (= :vertical-movement
                                                    (:rating rating)))
                                               (-> digram :ratings))))))
       (map (fn [digram]
              (-> digram :propability first))))


  (->> (describe-layout-rating hybrid-statistics
                               @(:layout-atom named-layout-atom-1))
       :distributions
       :digrams
       (sort-by (fn [digram]
                  (-> digram :propability second))
                #(compare %2 %1)))

  (->> (describe-layout-rating hybrid-statistics
                               @(:layout-atom named-layout-atom-1))
       :distributions
       :digrams
       (map (fn [digram]
              (meldey/find-first (fn [rating]
                                   (= :vertical-movement
                                      (:rating rating)))
                                 (-> digram :ratings))))

       (map :label)
       (frequencies))

  ;; => {:different-hand 125,
  ;;     :same-row 66,
  ;;     :different-finger-one-row-leap 61,
  ;;     :same-finger-one-row-leap 15,
  ;;     :different-finger-two-row-leap 6}

  ;; => {:different-hand 125,
  ;;     :same-row 66,
  ;;     :different-finger-one-row-leap 67,
  ;;     :same-finger-one-row-leap 9,
  ;;     :different-finger-two-row-leap 6}

  )

(defonce optimized-layouts-with-multipliers [])

(def common-named-layouts [{:name "qwerty"
                            :layout qwerty}
                           {:name "dvorak"
                            :layout dvorak}
                           {:name "colemak dh"
                            :layout colemak-dh}
                           {:name "ga-and-gd-optimized"
                            :layout ga-and-gd-optimized-hybrid-layout}
                           {:name "random"
                            :layout (random-layout (keys (:character-distribution hybrid-statistics)))}
                           ;; english-layout
                           ;; hybrid-layout

                           ;; (-> hybrid-layout
                           ;;     (assoc :name "customized-hybrid")
                           ;;     (update :layout
                           ;;             swap-mappings
                           ;;             {:character "r", :cocoa-key-code 34}
                           ;;             {:character "l", :cocoa-key-code 41}))
                           ])

(defn layout-comparison-views []
  (let [layouts (fn [statistics]
                  (binding [multipliers common-multipliers
                            #_(assoc multipliers :finger-type 0.1)]
                    (doall (for [layout (concat ;; (select-best-layouts 1 hybrid-statistics finnish-layouts)
                                         ;; (select-best-layouts 1 hybrid-statistics english-layouts)
                                         ;; (select-best-layouts 1 hybrid-statistics @hybrid-layouts-atom)
                                         ;; [best-hybrid-layout]
                                         @optimized-layouts-atom
                                         common-named-layouts
                                         optimized-layouts-with-multipliers
                                         (->> @optimization-history-atom
                                              last
                                              :ratings
                                              (sort-by second)
                                              (map first)
                                              (take 3)
                                              (map-indexed (fn [index layout]
                                                             {:name (str "optimized-" index)
                                                              :layout layout})))
                                         [])]
                             (assoc layout
                                    :layout-rating-description (describe-layout-rating statistics
                                                                                       ;;finnish-statistics
                                                                                       (:layout layout)))))))]
    (layouts/superimpose (visuals/rectangle-2 {:fill-color [0 0 0 255]})
                         (layouts/vertically-2 {:margin 20}
                                               (layout-comparison-text "english statistics")
                                               [#'layout-rating-comparison-view english-statistics (layouts english-statistics)]
                                               (layout-comparison-text "finnish statistics")
                                               [#'layout-rating-comparison-view finnish-statistics (layouts finnish-statistics)]
                                               (layout-comparison-text "hybrid statistics")
                                               [#'layout-rating-comparison-view hybrid-statistics (layouts hybrid-statistics)]))))

(defn start-layout-comparison-view []
  (start-view (fn []
                [#'layout-comparison-views])))

(comment
  ;; hot-right-now TODO: remove me
  (start-layout-comparison-view)
  )


(comment

  (spit "temp/optimized-layouts.edn" @optimized-layouts-atom)

  (swap! optimized-layouts-atom drop-last)
  (reset! optimized-layouts-atom [])

  (.start (Thread. (fn []
                     (doall (repeatedly 1
                                        (fn []
                                          (swap! optimized-layouts-atom
                                                 conj
                                                 (optimize-named-layout-with-multipliers common-multipliers
                                                                                         hybrid-statistics))))))))






  (binding [multipliers multipliers]
    (sort (map (partial rate-layout hybrid-statistics)
               (map :layout #_hybrid-layouts-2 [best-hybrid-layout]))))

  (describe-layout-rating finnish-statistics
                          (first @optimized-layouts-atom))

  (spit "temp/optimized-layouts-2.edn"
        (pr-str @optimized-layouts-atom))

  (edn/read-string (slurp "temp/optimized-layouts.edn"))

  (describe-layout-rating english-statistics
                          (:layout (first @optimized-layouts-atom)))

  (start-layout-comparison-view)


  (.start (Thread. (fn [] (def optimized-layouts-with-multipliers (doall (let [parameter-sets (for [ ;; roll [0 1 2]
                                                                                                    ;; key-rating [0 1 2]
                                                                                                    ;; finger-type [0 1 2]
                                                                                                    ;; horizontal-movement [0 1 2]
                                                                                                    ;; vertical-movement [0 1 2]

                                                                                                    digram-roll [1]
                                                                                                    trigram-roll [1]
                                                                                                    key [0.5 1 2]
                                                                                                    movement [1]
                                                                                                    hand-balance [0 0.5 1]
                                                                                                    statistics [#_english-statistics-with-diacritics
                                                                                                                #_finnish-statistics
                                                                                                                hybrid-statistics]]
                                                                                                {:statistics statistics
                                                                                                 :multipliers {:digram-roll digram-roll
                                                                                                               :trigram-roll trigram-roll
                                                                                                               :key-rating key
                                                                                                               :finger-type key
                                                                                                               :horizontal-movement movement
                                                                                                               :vertical-movement movement
                                                                                                               :hand-balance hand-balance}})]
                                                                           (for [parameters (take 300 parameter-sets)]
                                                                             (do (prn (-> parameters :multipliers) (-> parameters :statistics :name))
                                                                                 (optimize-named-layout-with-multipliers (:multipliers parameters)
                                                                                                                         (:statistics parameters))))))))))

  (spit "temp/optimized-layouts-with-multipliers-2" (pr-str optimized-layouts-with-multipliers))
  (def optimized-finnish-layout (gradient-descent-all finnish-statistics
                                                      (random-layout)))

  (binding [digram-roll-mulptiplier 1
            key-rating-multiplier 2]
    (let [file-name-body (str "temp/roll-" digram-roll-mulptiplier "-key-" key-rating-multiplier)]
      ;; (def optimized-english-layout (gradient-descent-all english-statistics
      ;;                                                     (random-layout)))

      ;; (def optimized-hybrid-layout (gradient-descent-all hybrid-statistics
      ;;                                                    (random-layout)))

      (spit (str file-name-body "english-text-english-layout.txt")
            (rating-description-to-text english-statistics
                                        optimized-english-layout))

      (spit (str file-name-body "-english-text-hybrid-layout.txt")
            (rating-description-to-text english-statistics
                                        optimized-hybrid-layout))

      (spit (str file-name-body "-finnish-text-hybrid-layout.txt")
            (rating-description-to-text finnish-statistics
                                        optimized-hybrid-layout))

      ;; (spit (str file-name-body "-english-layout.edn")
      ;;       (pr-str optimized-english-layout))

      ;; (spit (str file-name-body "-hybrid-layout.edn")
      ;;       (pr-str optimized-hybrid-layout))
      ))

  (binding [digram-roll-mulptiplier 1
            key-rating-multiplier 2]
    (let [file-name-body (str "temp/roll-" digram-roll-mulptiplier "-key-" key-rating-multiplier)]
      (spit (str file-name-body "-english-text-qwerty-layout.txt")
            (rating-description-to-text english-statistics
                                        qwerty))

      (spit (str file-name-body "-finnish-text-qwerty-layout.txt")
            (rating-description-to-text finnish-statistics
                                        qwerty))

      (spit (str file-name-body "-english-text-colemak-dh-layout.txt")
            (rating-description-to-text english-statistics
                                        colemak-dh))

      (spit (str file-name-body "-finnish-text-colemak-dh-layout.txt")
            (rating-description-to-text finnish-statistics
                                        colemak-dh))

      (spit (str file-name-body "-english-text-dvorak-layout.txt")
            (rating-description-to-text english-statistics
                                        dvorak))

      (spit (str file-name-body "-finnish-text-dvorak-layout.txt")
            (rating-description-to-text finnish-statistics
                                        dvorak))))

  (do (def optimized-hybrid-layout (gradient-descent-all hybrid-statistics
                                                         (random-layout)))
      (print-rows 29
                  (rating-description-to-rows (describe-layout-rating english-statistics
                                                                      optimized-hybrid-layout))))

  (binding [digram-roll-mulptiplier 1
            key-rating-multiplier 2]
    (for [statistics [[:english english-statistics]
                      [:finnish finnish-statistics]
                      [:hybrid hybrid-statistics]]
          layout     [[:english optimized-english-layout]
                      [:finnish optimized-finnish-layout]
                      [:hybrid optimized-hybrid-layout]
                      [:qwerty qwerty]
                      [:dvorak dvorak]
                      [:colemak-dh colemak-dh]]]
      [(first statistics)
       (first layout)
       (rate-layout (second statistics)
                    (second layout))]))

  (spit "temp/rating.csv"
        (csv-rows-to-string (rating-description-to-rows (describe-layout-rating english-statistics
                                                                                optimized-hybrid-layout))))

  (map vector
       (first (sort-text-statistics english-statistics))
       (first (sort-text-statistics finnish-statistics)))

  (let [sample (fn [statistics]
                 (set (take 8 (map first (first (sort-text-statistics statistics))))))]
    (set/difference
     (set/union (sample english-statistics)
                (sample finnish-statistics))
     (set/intersection (sample english-statistics)
                       (sample finnish-statistics))))


  (def target-text-statistics (text-statistics target-text))
  (do (def optimized-layouts (doall (repeatedly 5
                                                (fn []
                                                  (gradient-descent-all english-statistics
                                                                        (random-layout))))))

      (map (partial rate-layout english-statistics)
           optimized-layouts))

  ;; => (-0.7801370918131306 -0.7816704209751654)


  (describe-layout-rating target-text-statistics
                          (nth optimized-layouts
                               0))

  (set/difference (into #{} (map :character (first optimized-layouts)))
                  (into #{} (map first (:character-distribution target-text-statistics))))

  )

(defn select-best-layouts [count statistics layouts]
  (take count (sort-by (fn [named-layout]
                         (rate-layout statistics
                                      (:layout named-layout)))
                       layouts)))
(comment
  (binding [multipliers multipliers]
    (doall (map layout-name (select-best-layouts 30 hybrid-statistics @hybrid-layouts-atom))))


  (binding [multipliers common-multipliers]
    (doall (map (partial rate-layout hybrid-statistics)
                (map :layout (select-best-layouts 200 hybrid-statistics @hybrid-layouts-atom)))))
  )

(defn digram-test-view [layout _text-statistics _highlighted-characters]
  (let [state-atom (dependable-atom/atom {:highlighted-characters #{}})
        cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)
        character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)
        key-colors-for-fingers (key-colors-for-fingers cocoa-key-code-to-character)]
    (fn [layout text-statistics highlighted-characters]
      (let [character-color (into {}
                                  (concat (for [highlighted-character highlighted-characters]
                                            [highlighted-character [70 100 70 255]])
                                          (for [highlighted-character (:highlighted-characters @state-atom)]
                                            [highlighted-character [100 150 70 255]])))]
        (layouts/vertically-2 {:margin 10}
                              [keyboard-view cocoa-key-code-to-character
                               (merge key-colors-for-fingers
                                      character-color)]
                              (text (format "%.3f"
                                            (float (rate-layout text-statistics
                                                                layout))))
                              (digram-view (:digram-distribution text-statistics)
                                           character-to-cocoa-key-code
                                           (fn [digram]
                                             (swap! state-atom assoc :highlighted-characters (or digram
                                                                                                 #{})))))))))


(comment
  (start-view (fn []
                (layouts/horizontally-2 {:margin 10}
                                        (for [optimized-layout (take 5 optimized-layouts)]
                                          [digram-test-view
                                           optimized-layout
                                           (normalized-digram-distribution target-text)
                                           #{}]))))

  (start-view (fn []
                [digram-test-view
                 trigram-english
                 (:digram-distribution english-statistics)
                 #{}]))
  ) ;; TODO: remove me



(defn text-test-view [layout _text]
  (let [state-atom (dependable-atom/atom {:highlighted-character nil})
        layout-rating (rate-layout (text-statistics text)
                                   layout)]
    (fn [layout test-text]
      (let [cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)
            character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)]
        (layouts/vertically-2 {:margin 10}
                              [keyboard-view
                               cocoa-key-code-to-character
                               (merge (key-colors-for-fingers cocoa-key-code-to-character)
                                      {(:highlighted-character @state-atom) [200 150 70 255]})]
                              (text (str "layout rating for the text: " layout-rating))
                              (layouts/horizontally-2 {:margin 20}
                                                      (layouts/vertically-2 {}
                                                                            (text "character")
                                                                            (text "finger")
                                                                            (text "row")
                                                                            (text "hand")
                                                                            (text "key rating")
                                                                            (text "key pair rating"))
                                                      (layouts/flow (map (partial character-view
                                                                                  character-to-cocoa-key-code
                                                                                  (fn [character-under-mouse]
                                                                                    (prn 'character-under-mouse character-under-mouse) ;; TODO: remove me

                                                                                    (swap! state-atom assoc :highlighted-character character-under-mouse)))
                                                                         (partition-all 2 1 (map str test-text))))))))))
(comment
  (let [test-text "kotivara vetää myynnistä lisää makkaroita"]
    (start-view (fn []
                  (layouts/vertically-2 {:margin 10}
                                        ;; [#'text-test-view
                                        ;;  qwerty
                                        ;;  test-text]
                                        ;; [#'text-test-view
                                        ;;  (edn/read-string (slurp "optimized-layout-for-finnish-and-english.edn"))
                                        ;;  test-text]
                                        [#'text-test-view
                                         optimized-layout
                                         test-text]
                                        ;; [#'text-test-view
                                        ;;  (edn/read-string (slurp "optimized-layout-for-finnish.edn"))
                                        ;;  test-text]
                                        ))))
  ) ;; TODO: remove me


;; (defn layout-view [_layout]
;;   (let [state-atom (dependable-atom/atom {:pressed-java-key-codes #{}
;;                                           :text ""})]
;;     (fn [layout]
;;       (let [cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)
;;             character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)
;;             digram-distribution (digram-distribution (filter-target-text target-text))
;;             highlighted-characters #_(into #{} (map str target-text))
;;             #_(into #{} (map :character (set/intersection optimized-layout
;;                                                           qwerty)))
;;             (->> @state-atom
;;                  (:pressed-java-key-codes)
;;                  (map java-key-code-to-cocoa-key-code)
;;                  (map cocoa-key-code-to-character)
;;                  (into #{}))]
;;         (assoc (layouts/vertically-2 {:margin 10}
;;                                      (layouts/flow (map (partial character-view character-to-cocoa-key-code)
;;                                                         (partition-all 2 1 (map str (take 100 (string/lower-case target-text)))))))
;;                :keyboard-event-handler (fn [_subtree event]
;;                                          (when (and (= :key-pressed (:type event))
;;                                                     (= :back-space (:key event)))
;;                                            (swap! state-atom update :text (fn [text]
;;                                                                             (apply str (drop-last text)))))
;;                                          (when (and (= :key-pressed (:type event))
;;                                                     (not (= 0 (:key-code event)))
;;                                                     (not (= 17 (:key-code event))) ;; control
;;                                                     (not (= 16 (:key-code event))) ;; shift
;;                                                     (not (= 157 (:key-code event))) ;; meta
;;                                                     (not (= :back-space (:key event))))
;;                                            (swap! state-atom update :pressed-java-key-codes conj (:key-code event))
;;                                            (swap! state-atom update :text str (or (cocoa-key-code-to-character (java-key-code-to-cocoa-key-code (:key-code event)))
;;                                                                                   (:character event))))
;;                                          (when (= :key-released (:type event))
;;                                            (swap! state-atom update :pressed-java-key-codes disj (:key-code event))))
;;                :can-gain-focus? true)))))

(defonce latest-optimized-layout-atom (atom {:name "random layout"
                                             :layout (random-layout)}))

(defonce former-optimized-layouts-atom (atom (read-string (slurp "temp/former-optimized-layouts.edn"))))

(comment
  ;; hot-right-now TODO: remove me
  (reset! latest-optimized-layout-atom {:name "random layout"
                                        :layout (random-layout)})

  (reset! former-optimized-layouts-atom [])

  (spit "temp/latest-optimized-layout.edn" (pr-str @latest-optimized-layout-atom))


  (spit "temp/former-optimized-layouts.edn" (pr-str (conj @former-optimized-layouts-atom
                                                          @latest-optimized-layout-atom)))

  ) ;; TODO: remove me


(defn start-optimization [state-atom text-statistics]
  (.start (Thread. (fn []
                     (loop []
                       (let [new-named-layout (-> (optimize-named-layout-with-multipliers multipliers
                                                                                          #_{:digram-roll 1
                                                                                             :trigram-roll 0
                                                                                             :key-rating 1
                                                                                             :finger-type 0.1
                                                                                             :horizontal-movement 0.1
                                                                                             :vertical-movement 1
                                                                                             :hand-balance 0.1}
                                                                                          text-statistics
                                                                                          {:initial-layout (:layout (last (:layouts @state-atom)))})
                                                  (assoc :optimizatoin-round (:optimization-round @state-atom)))

                             #_(optimize text-statistics
                                         (:layout (last (:layouts @state-atom))))]


                         (when (not (= (:layout new-named-layout)
                                       (:layout (last (:layouts @state-atom)))))

                           (swap! state-atom assoc :last-round-when-improvement-was-found (:optimization-round @state-atom))

                           (reset! latest-optimized-layout-atom new-named-layout)

                           (swap! state-atom
                                  update
                                  :layouts (fn [layouts]
                                             (take-last 4
                                                        (concat layouts
                                                                [new-named-layout]))))))

                       (swap! state-atom update :optimization-round inc)

                       (async/>!! @event-channel-atom
                                  {:type :foo})
                       (when (:optimize? @state-atom)
                         (recur)))))))

(defn optimization-view [_text-statistics]
  (let [state-atom (dependable-atom/atom {:optimization-round 0
                                          :layouts [@latest-optimized-layout-atom]
                                          :optimize? false})]
    (fn [text-statistics]
      (let [state @state-atom]
        (black-background
         (layouts/vertically-2 {:margin 10}
                               (button/button (if (:optimize? state)
                                                "stop optimizing"
                                                "start optimizing")
                                              (fn []
                                                (swap! state-atom update :optimize? not)
                                                (when (:optimize? @state-atom)
                                                  (start-optimization state-atom
                                                                      text-statistics))))
                               (text (str "optimization round:" (:optimization-round @state-atom)))
                               (text (str "rounds since improvement:" (- (:optimization-round @state-atom)
                                                                         (or (:last-round-when-improvement-was-found @state-atom)
                                                                             0))))
                               (text (str "last round-when improvement was found:" (:last-round-when-improvement-was-found @state-atom)))

                               [layout-rating-comparison-view text-statistics (concat common-named-layouts
                                                                                      (:layouts state))]
                               ;; (layouts/horizontally-2 {:margin 10}
                               ;;                         (concat [[digram-test-view
                               ;;                                   (first (:layouts state))
                               ;;                                   text-statistics
                               ;;                                   #{}]]
                               ;;                                 (for [[previous-layout layout] (partition 2 1 (:layouts state))]
                               ;;                                   [digram-test-view
                               ;;                                    layout
                               ;;                                    text-statistics
                               ;;                                    (set/difference layout-characters
                               ;;                                                    (set (map :character (set/intersection previous-layout
                               ;;                                                                                           layout))))])))
                               ;; [#'text-test-view
                               ;;  (last (:layouts state))
                               ;;  "kotivara vetää myynnistä lisää makkaroita"]
                               ))))))

(comment
  ;; hot-right-now TODO: remove me
  (start-view (fn [] [#'optimization-view hybrid-statistics])
              #_{:join? true})
  ) ;; TODO: remove me

(defn distribution-view [distribution digrams]
  (let [max-propability (apply max (vals distribution))]
    (layouts/vertically-2 {:margin 2}
                          (for [digram digrams]
                            {:node (visuals/rectangle-2 {:fill-color [128 128 128 255]})
                             :width (* 700
                                       (/ (or (distribution digram)
                                              0)
                                          max-propability))
                             :height 2}))))

(defn digram-distribution-comparison-view []
  (layouts/with-margin 10
    (let [texts [(str (subs (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                            0 300000)
                      (subs (slurp "temp/text/the-hacker-crackdown.txt")
                            0 300000))
                 (key-log-to-string key-log-file-path)]
          digram-distributions (map normalized-digram-distribution texts)
          character-distributions (map normalized-character-distribution texts)]
      (layouts/vertically-2 {:margin 10}
                            (layouts/horizontally-2 {:margin 10}
                                                    (for [digram-distribution digram-distributions]
                                                      (distribution-view digram-distribution
                                                                         (reverse (sort-by (apply merge (reverse digram-distributions))
                                                                                           (apply set/union
                                                                                                  (map set
                                                                                                       (map #(map first %)
                                                                                                            digram-distributions))))))))
                            (layouts/horizontally-2 {:margin 10}
                                                    (for [character-distribution character-distributions]
                                                      (distribution-view character-distribution
                                                                         (reverse (sort-by (apply merge (reverse character-distributions))
                                                                                           (set (mapcat keys character-distributions)))))))))))


(comment
  (start-view #'digram-distribution-comparison-view)
  ) ;; TODO: remove me


(defn start []
  (println "\n\n------------ start -------------\n\n")
  (start-layout-comparison-view)
  ;; (reset! event-channel-atom
  ;;         (application/start-application (fn []
  ;;                                          [#'optimization-view target-text])
  ;;                                        ;; #'digram-distribution-comparison-view
  ;;                                        :on-exit #(reset! event-channel-atom nil)))
  )



(refresh-view!)

(comment

  (spit "optimized-layout-for-finnish-and-english.edn" (pr-str optimized-layout))
  (spit "qwerty.edn" (pr-str qwerty))
  (def optimized-layout (edn/read-string (slurp "optimized-layout.edn")))
  ) ;; TODO: remove me


;; TODO: show character distribution heat map in editor
;; color keys by highest gain possible by swapping them to another key
;; allow comparing other metrics than total effort
;; show effort difference if the selected key would be swapped with the key under cursor
