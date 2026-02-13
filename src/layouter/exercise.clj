(ns layouter.exercise
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [flow-gl.gui.visuals :as visuals]
   [fungl.color :as color]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [layouter.gui :as gui]
   [layouter.keyboard :as keyboard]
   [layouter.keyboard-view :as keyboard-view]
   [layouter.layout :as layout]
   [layouter.text :as text]
   [layouter.view :as view]
   [medley.core :as medley])
  (:import
   [java.util.zip ZipInputStream]))

(defn read-zipped-text-file
  "Reads the first text file from a ZIP archive and returns its content as a string."
  [zip-file-path]
  (with-open [zip (ZipInputStream. (io/input-stream zip-file-path))]
    (loop [entry (.getNextEntry zip)]
      (when entry
        (if (.isDirectory entry)
          (recur (.getNextEntry zip))
          (slurp (io/reader zip)))))))

(defn create-zip-from-string
  "Creates a ZIP file containing a single text file with the given string content."
  [output-zip-path file-name-in-zip content]
  (with-open [zip (java.util.zip.ZipOutputStream. (io/output-stream output-zip-path))]
    (.putNextEntry zip (java.util.zip.ZipEntry. file-name-in-zip))
    (.write zip (.getBytes content "UTF-8"))
    (.closeEntry zip)))

(defonce english-words (->> (io/resource "english-words.txt.zip")
                            (read-zipped-text-file)
                            (.getBytes)
                            (io/reader)
                            (line-seq)))

(defonce finnish-words (-> (io/resource "nykysuomensanalista.txt.zip")
                           (read-zipped-text-file)
                           (.getBytes)
                           (io/reader)
                           (line-seq)))

(defn characters-from-common-to-rare [character-distribution]
  (->> character-distribution
       (sort-by second)
       (reverse)
       (map first)))

(defn take-most-common-characters [number-of-used-characters character-distribution]
  (->> (characters-from-common-to-rare character-distribution)
       (take number-of-used-characters)))

(deftest test-take-most-common-characters
  (is (= '(:c :b)
         (take-most-common-characters 2
                                      {:a 0.1
                                       :b 0.2
                                       :c 0.3}))))

(defn filter-words-by-characters [allowed-characters word-list]
  (let [allowed-characters-set (set (map first allowed-characters))]
    (->> word-list
         (filter (fn [word]
                   (and (string/includes? word (last allowed-characters))
                        (every? allowed-characters-set (seq word))))))))

(deftest test-filter-words-by-characters
  (is (= '("ab")
         (filter-words-by-characters ["a" "b"]
                                     ["abc"
                                      "ab"
                                      "aa"]))))

(defn excericise-word-for-characters [characters]
  (first (filter-words-by-characters characters
                                     (shuffle (concat english-words
                                                      #_finnish-words)))))

(defn excericise-word-from-text-statistics [text-statistics number-of-characters]
  (first (filter-words-by-characters (take-most-common-characters number-of-characters
                                                                  (:character-distribution text-statistics))
                                     (shuffle (concat english-words
                                                      finnish-words)))))

(defn layout-exercise-view [all-characters target-word-for-characters _layout]
  (let [generate-exercise-word (fn [character-count]
                                  (target-word-for-characters (take character-count all-characters)))
        state-atom (dependable-atom/atom {:xp 0
                                          :next-level-xp 50
                                          :next-level-xp-increment 50
                                          :character-count 4
                                          :typed-text ""
                                          :cocoa-key-code-down nil
                                          :target-word (generate-exercise-word 4)})]
    (fn [all-characters _target-word-for-characters layout]
      (let [cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character layout)
            character-to-cocoa-key-code (layout/layout-to-character-to-cocoa-key-code layout)
            selected-characters (take (:character-count @state-atom)
                                      all-characters)]

        {:node (layouts/vertically-2 {:margin 10 :centered? true}
                                     (gui/text (:target-word @state-atom))
                                     (gui/text (:typed-text @state-atom))
                                     (keyboard-view/keyboard-view cocoa-key-code-to-character
                                                                  (merge keyboard-view/key-colors-for-fingers
                                                                         (-> (into {}
                                                                                   (for [character (set/difference (set (keys character-to-cocoa-key-code))
                                                                                                                   (set selected-characters))
                                                                                         #_(drop-last selected-characters)]
                                                                                     [(character-to-cocoa-key-code character)
                                                                                      (let [[r g b _a] (keyboard-view/key-colors-for-fingers (character-to-cocoa-key-code character))]
                                                                                        [r g b 0.6]
                                                                                        #_[0 0 0 255])]))
                                                                             (assoc (character-to-cocoa-key-code (last selected-characters))
                                                                                    [0.5 0 0 255])
                                                                             (assoc (:cocoa-key-code-down @state-atom)
                                                                                    [0 0.8 0 255]))))
                                     (gui/text (str (:character-count @state-atom)
                                                    " / "
                                                    (last selected-characters)))
                                     (gui/text (str (:xp @state-atom)
                                                    " / "
                                                    (:next-level-xp @state-atom))))
         :can-gain-focus? true
         :keyboard-event-handler (fn [_subtree event]
                                   (when (= :key-released
                                            (:type event))
                                     (swap! state-atom dissoc :cocoa-key-code-down))

                                   (when (= :key-pressed
                                            (:type event))

                                     (when (= :back-space (:key event))
                                       (swap! state-atom update :typed-text (fn [typed-text]
                                                                              (string/join (drop-last typed-text)))))

                                     (when (= :space (:key event))
                                       (swap! state-atom assoc :cocoa-key-code-down (character-to-cocoa-key-code (str (nth (:target-word @state-atom)
                                                                                                                           (count (:typed-text @state-atom))))))
                                       (swap! state-atom (fn [state]
                                                           (-> state
                                                               (assoc :typed-text (subs (:target-word state)
                                                                                        0
                                                                                        (inc (count (:typed-text state)))))))))

                                     (when (and (= :up (:key event))
                                                (> (count layout)
                                                   (:character-count @state-atom)))
                                       (swap! state-atom (fn [state]
                                                           (let [new-character-count (if (:shift? event)
                                                                                       (count layout)
                                                                                       (inc (:character-count state)))]
                                                             (assoc state
                                                                    :character-count new-character-count
                                                                    :typed-text ""
                                                                    :target-word (generate-exercise-word new-character-count))))))
                                     (when (and (= :down (:key event))
                                                (< 2 (:character-count @state-atom)))
                                       (swap! state-atom (fn [state]
                                                           (let [new-character-count (if (:shift? event)
                                                                                       2
                                                                                       (dec (:character-count state)))]
                                                             (assoc state
                                                                    :character-count new-character-count
                                                                    :typed-text ""
                                                                    :target-word (generate-exercise-word new-character-count))))))

                                     (when-some [character (cocoa-key-code-to-character (keyboard/java-key-code-to-cocoa-key-code (:key-code event) ))]
                                       (swap! state-atom assoc :cocoa-key-code-down (keyboard/java-key-code-to-cocoa-key-code (:key-code event)))
                                       (swap! state-atom update :typed-text str character))

                                     (when (= (:target-word @state-atom)
                                              (:typed-text @state-atom))
                                       (swap! state-atom (fn [state]
                                                           (-> state
                                                               (assoc :typed-text ""
                                                                      :target-word (generate-exercise-word (:character-count @state-atom))))

                                                           #_(-> state
                                                                 (assoc :xp (+ (:xp state)
                                                                               (count (:target-word state)))
                                                                        :typed-text ""
                                                                        :target-word (generate-exercise-word (:character-count @state-atom)))
                                                                 (cond-> (<= (:next-level-xp state)
                                                                             (+ (:xp state)
                                                                                (count (:target-word state))))
                                                                   (assoc :next-level-xp (+ (:next-level-xp state)
                                                                                            (:next-level-xp-increment state))
                                                                          :character-count (inc (:character-count state))
                                                                          :next-level-xp-increment (+ 10
                                                                                                      (:next-level-xp-increment state))))))))))}))))


(def maximum-duration 5000)
(def minimum-duration 1000)
(def active-character-count 4)

(defn next-target-character [state-atom durations-atom character-distribution]
  (let [characters (map first character-distribution)
        target-character (let [remaining-characters (->> characters
                                                         (remove (fn [character]
                                                                   (or (= (:target-character @state-atom)
                                                                          character)
                                                                       (> 1000
                                                                          (get @durations-atom
                                                                               character
                                                                               maximum-duration))))))]
                           (->> remaining-characters
                                (concat (take (max 0 (- active-character-count
                                                        (count remaining-characters)))
                                              (shuffle (remove (fn [character]
                                                                 (= (:target-character @state-atom)
                                                                    character))
                                                               characters))))
                                (sort-by (fn [character]
                                           [(get @durations-atom
                                                 character
                                                 maximum-duration)
                                            (- 1
                                               (get character-distribution
                                                    character))]))
                                (take active-character-count)
                                (rand-nth)))
        current-duration (when (:start-time @state-atom)
                           (min maximum-duration
                                (- (System/currentTimeMillis)
                                   (:start-time @state-atom))))]

    (when (:target-character @state-atom)
      (swap! durations-atom
             update
             (:target-character @state-atom)
             (fn [duration]
               (double (min maximum-duration
                            (/ (+ (or duration maximum-duration)
                                  current-duration)
                               2))))))
    (swap! state-atom assoc
           :previous-duration current-duration
           :previous-target-character (:target-character @state-atom)
           :start-time (System/currentTimeMillis)
           :target-character target-character)

    target-character))

(def ready-color (conj (color/hsluv-to-rgb 135 1.0 0.4) 1.0))
(def almost-ready-color (conj (color/hsluv-to-rgb 67 1.0 0.4) 1.0))
(def unready-color (conj (color/hsluv-to-rgb 0 0.0 0.4) 1.0))

(defn duration-color [duration]
  (if (< duration minimum-duration)
    ready-color
    unready-color))

(defn duration-cell [character duration]
  (let [width 200]
    (layouts/with-margin 10
      (layouts/with-minimum-size width nil
        (layouts/superimpose (assoc (visuals/rectangle-2 {:fill-color (duration-color duration)})
                                    :height gui/font-size
                                    :width (* width
                                              (abs (/ duration
                                                      maximum-duration))))
                             (gui/text (str character ":" duration)
                                       {:color [200 200 200 255]}))))))

(def durations-file-path "temp/durations-edn")

(defn character-exercise-view [durations-atom character-distribution _layout]
  (let [state-atom (dependable-atom/atom {:typed-character ""
                                          :cocoa-key-code-down nil})]
    (swap! state-atom assoc :target-character (next-target-character state-atom durations-atom character-distribution))
    (fn [durations-atom character-distribution layout]
      (let [cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character layout)]
        {:node (layouts/vertically-2 {:margin 20 :centered? true}
                                     (gui/text (:target-character @state-atom))
                                     (gui/text (:typed-character @state-atom))
                                     (when (:previous-target-character @state-atom)
                                       (let [duration (get @durations-atom (:previous-target-character @state-atom))]
                                         (layouts/box 5
                                                      (visuals/rectangle-2 {:fill-color (cond (< duration
                                                                                                 minimum-duration)
                                                                                              ready-color

                                                                                              (< (:previous-duration @state-atom)
                                                                                                 minimum-duration)
                                                                                              almost-ready-color

                                                                                              :else
                                                                                              unready-color)})
                                                      (gui/text (str (:previous-target-character @state-atom) ": " (:previous-duration @state-atom) " -> " (int duration))
                                                                #_{:color (duration-color duration)}))))
                                     (keyboard-view/keyboard-view cocoa-key-code-to-character
                                                                  (assoc keyboard-view/key-colors-for-fingers
                                                                         (:cocoa-key-code-down @state-atom)
                                                                         [0 0.8 0 255]))
                                     (layouts/with-maximum-size 1000 nil
                                       (layouts/flow (for [character (->> character-distribution
                                                                          (sort-by second)
                                                                          (reverse)
                                                                          (map first))]
                                                       (duration-cell character (int (get @durations-atom character maximum-duration)))))))
         :can-gain-focus? true
         :keyboard-event-handler (fn [_subtree event]
                                   (when (= :key-released
                                            (:type event))
                                     (swap! state-atom dissoc :cocoa-key-code-down))

                                   (when (= :key-pressed
                                            (:type event))

                                     (when-some [character (cocoa-key-code-to-character (keyboard/java-key-code-to-cocoa-key-code (:key-code event) ))]
                                       (swap! state-atom assoc
                                              :cocoa-key-code-down (keyboard/java-key-code-to-cocoa-key-code (:key-code event))
                                              :typed-character character))

                                     (when (= (:target-character @state-atom)
                                              (:typed-character @state-atom))
                                       (swap! state-atom update :characters-after-save (fnil inc 0))
                                       (when (< (:characters-after-save @state-atom)
                                                10)
                                         #_(spit durations-file-path (pr-str @durations-atom))
                                         (swap! state-atom assoc :characters-after-save 0))
                                       (swap! state-atom assoc
                                              :target-character (next-target-character state-atom durations-atom character-distribution )
                                              :typed-character ""))))}))))

(defn make-sequencer [sequence]
  (let [state (atom (cycle sequence))]
    (fn []
      (let [current @state]
        (swap! state rest)
        (first current)))))

(defn make-exercise-word-generator-from-text [text]
  (make-sequencer (string/split text #"\s")))

(defn layout-demo-view [sample-text _layout]
  (let [generate-exercise-word (make-exercise-word-generator-from-text sample-text)
        state-atom (dependable-atom/atom {:typed-text ""
                                          :cocoa-key-code-down nil
                                          :target-word (generate-exercise-word)})
        qwerty-cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character layout/qwerty)]
    (fn [_sample-text layout]
      (let [cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character layout)
            character-to-cocoa-key-code (layout/layout-to-character-to-cocoa-key-code layout)]

        {:node (layouts/vertically-2 {:margin 10 :centered? true}
                                     (gui/text (:target-word @state-atom))
                                     (layouts/horizontally-2 {}
                                                             (gui/text (apply str
                                                                              (map (comp qwerty-cocoa-key-code-to-character
                                                                                         character-to-cocoa-key-code)
                                                                                   (map str (:typed-text @state-atom))))
                                                                       {:color [0.8 1.0 0.8 1.0]})
                                                             (gui/text (apply str
                                                                              (map (comp qwerty-cocoa-key-code-to-character
                                                                                         character-to-cocoa-key-code)
                                                                                   (map str (subs (:target-word @state-atom)
                                                                                                  (min (count (:target-word @state-atom))
                                                                                                       (count (:typed-text @state-atom)))))))))

                                     (gui/text (:typed-text @state-atom))
                                     (keyboard-view/keyboard-view cocoa-key-code-to-character
                                                                  (merge (medley/map-vals (partial gui/multiply-color 0.5)
                                                                                          keyboard-view/key-colors-for-fingers)
                                                                         (into {}
                                                                               (for [cocoa-key-code (->> @state-atom
                                                                                                         :target-word
                                                                                                         distinct
                                                                                                         (map str)
                                                                                                         (map character-to-cocoa-key-code))]
                                                                                 [cocoa-key-code (gui/multiply-color 1.5 (get keyboard-view/key-colors-for-fingers
                                                                                                                              cocoa-key-code))]))
                                                                         {(:cocoa-key-code-down @state-atom)
                                                                          [0 0.8 0 255]})))
         :can-gain-focus? true
         :keyboard-event-handler (fn [_subtree event]
                                   (when (= :key-released
                                            (:type event))
                                     (swap! state-atom dissoc :cocoa-key-code-down))

                                   (when (= :key-pressed
                                            (:type event))

                                     (when (= :back-space (:key event))
                                       (swap! state-atom update :typed-text (fn [typed-text]
                                                                              (string/join (drop-last typed-text)))))

                                     (when (= :space (:key event))
                                       (when (< (count (:typed-text @state-atom))
                                                (count (:target-word @state-atom)))
                                         (swap! state-atom assoc :cocoa-key-code-down (character-to-cocoa-key-code (str (nth (:target-word @state-atom)
                                                                                                                             (count (:typed-text @state-atom)))))))
                                       (swap! state-atom (fn [state]
                                                           (-> state
                                                               (assoc :typed-text (subs (:target-word state)
                                                                                        0
                                                                                        (inc (count (:typed-text state)))))))))

                                     (when-some [cocoa-key-code (cocoa-key-code-to-character (keyboard/java-key-code-to-cocoa-key-code (:key-code event) ))]
                                       (swap! state-atom assoc :cocoa-key-code-down (keyboard/java-key-code-to-cocoa-key-code (:key-code event)))
                                       (swap! state-atom update :typed-text str cocoa-key-code))

                                     (when (= (:target-word @state-atom)
                                              (:typed-text @state-atom))
                                       (swap! state-atom (fn [state]
                                                           (assoc state
                                                                  :typed-text ""
                                                                  :target-word (generate-exercise-word)))))))}))))

(defn create-next-target-character [durations-atom character-distribution]
  (let [state-atom (atom {})
        maximum-duration 5000]
    (fn [characters]
      (let [target-character (let [remaining-characters (->> characters
                                                             (remove (fn [character]
                                                                       (or (= (:target-character @state-atom)
                                                                              character)
                                                                           (> 1000
                                                                              (get @durations-atom
                                                                                   character
                                                                                   maximum-duration))))))]
                               (->> remaining-characters
                                    (concat (take (max 0 (- 5 (count remaining-characters)))
                                                  (shuffle characters)))
                                    (sort-by (fn [character]
                                               [(get @durations-atom
                                                     character
                                                     maximum-duration)
                                                (- 1
                                                   (get character-distribution
                                                        character))]))
                                    (take 5)
                                    (rand-nth)))]
        (when (:target-character @state-atom)
          (swap! durations-atom
                 update
                 (:target-character @state-atom)
                 (fn [duration]
                   (double (min maximum-duration
                                (/ (+ (or duration maximum-duration)
                                      (- (System/currentTimeMillis)
                                         (:start-time @state-atom)))
                                   2)))))
          #_(prn (:durations @state-atom)))
        (swap! state-atom assoc
               :start-time (System/currentTimeMillis)
               :target-character target-character)

        target-character))))



(defonce durations-atom (atom (if (.exists (io/file durations-file-path))
                                (edn/read-string (slurp durations-file-path))
                                {})))

(defn english-demo-text [& [length]]
  (->> (-> "temp/text/the-hacker-crackdown.txt"
           slurp
           (subs 0 (or length 500))
           (string/replace #"\s+" " "))
       (map str)
       (map string/lower-case)
       (filter (conj (set text/english-characters)
                     " "))
       (apply str)))

(def layouts {:xaei {:layout (:layout layout/xaei)
                     :hue 250}
              :qwerty {:layout layout/qwerty
                       :hue 350}})

(def layout-key :xaei)

(defn colored-exercise-view [layout-key exercise-view]
  (layouts/superimpose (visuals/rectangle-2 {:fill-color (conj (color/hsluv-to-rgb (-> layouts layout-key :hue)
                                                                                   1.0
                                                                                   0.2)
                                                               1.0)})
                       (layouts/center (exercise-view (-> layouts layout-key :layout)))))


(defn colored-character-exercise-view []
  (colored-exercise-view layout-key
                         (fn [layout]
                           [#'character-exercise-view
                            (atom {})
                            (:character-distribution text/keyboard-design-com-english-text-statistics #_text/finnish-statistics)
                            layout])))

(defn colored-layout-exercise-view []
  (colored-exercise-view layout-key
                         (fn [layout]
                           [#'layout-exercise-view
                            (characters-from-common-to-rare (:character-distribution text/keyboard-design-com-english-text-statistics #_text/finnish-statistics))
                            excericise-word-for-characters
                            layout])))


(defn colored-layout-demo-view []
  (layouts/superimpose (visuals/rectangle-2 {:fill-color (conj (color/hsluv-to-rgb (-> layouts :qwerty :hue)
                                                                                   1.0
                                                                                   0.2)
                                                               1.0)})
                       (layouts/center [#'layout-demo-view
                                        (english-demo-text)
                                        (-> layouts layout-key :layout)])))


(comment
  (view/start-view #'colored-layout-demo-view)
  (view/start-view #'colored-character-exercise-view)
  (view/start-view #'colored-layout-exercise-view)
  )


(view/hard-refresh-view!)
