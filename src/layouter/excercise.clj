(ns layouter.excercise
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
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

(defn take-most-common-characters [number-of-used-characters character-distribution]
  (->> character-distribution
       (sort-by second)
       (reverse)
       (map first)
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

(defn excericise-word-from-text-statistics [text-statistics number-of-characters]
  (first (filter-words-by-characters (take-most-common-characters number-of-characters
                                                                  (:character-distribution text-statistics))
                                     (shuffle (concat english-words
                                                      finnish-words)))))

(defn layout-excercise-view [text-statistics _layout]
  (let [generate-excercise-word (partial excericise-word-from-text-statistics text-statistics)
        state-atom (dependable-atom/atom {:character-count 4
                                          :typed-text ""
                                          :cocoa-key-code-down nil
                                          :target-word (generate-excercise-word 4)})]
    (fn [text-statistics layout]
      (let [cocoa-key-code-to-character (layout/layout-to-cocoa-key-code-to-character layout)
            character-to-cocoa-key-code (layout/layout-to-character-to-cocoa-key-code layout)
            characters (take-most-common-characters (:character-count @state-atom)
                                                    (:character-distribution text-statistics))]

        {:node (layouts/vertically-2 {:margin 10 :centered? true}
                                     (gui/text (:target-word @state-atom))
                                     (gui/text (:typed-text @state-atom))
                                     (keyboard-view/keyboard-view cocoa-key-code-to-character
                                                                           (merge keyboard-view/key-colors-for-fingers
                                                                                  (-> (into {}
                                                                                            (for [character (set/difference (set (keys character-to-cocoa-key-code))
                                                                                                                            (set characters))
                                                                                                  #_(drop-last characters)]
                                                                                              [(character-to-cocoa-key-code character)
                                                                                               (let [[r g b _a] (keyboard-view/key-colors-for-fingers (character-to-cocoa-key-code character))]
                                                                                                 [r g b 0.6]
                                                                                                 #_[0 0 0 255])]))
                                                                                      (assoc (character-to-cocoa-key-code (last characters))
                                                                                             [0.5 0 0 255])
                                                                                      (assoc (:cocoa-key-code-down @state-atom)
                                                                                             [0 0.8 0 255]))))
                                     (gui/text (str (:character-count @state-atom)
                                                    " / "
                                                    (last characters))))
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
                                                                    :target-word (generate-excercise-word new-character-count))))))
                                     (when (and (= :down (:key event))
                                                (< 2 (:character-count @state-atom)))
                                       (swap! state-atom (fn [state]
                                                           (let [new-character-count (if (:shift? event)
                                                                                       2
                                                                                       (dec (:character-count state)))]
                                                             (assoc state
                                                                    :character-count new-character-count
                                                                    :typed-text ""
                                                                    :target-word (generate-excercise-word new-character-count))))))

                                     (when-some [character (cocoa-key-code-to-character (keyboard/java-key-code-to-cocoa-key-code (:key-code event) ))]
                                       (swap! state-atom assoc :cocoa-key-code-down (keyboard/java-key-code-to-cocoa-key-code (:key-code event)))
                                       (swap! state-atom update :typed-text str character))

                                     (when (= (:target-word @state-atom)
                                              (:typed-text @state-atom))
                                       (swap! state-atom (fn [state]
                                                           (assoc state
                                                                  :typed-text ""
                                                                  :target-word (generate-excercise-word (:character-count @state-atom))))))))}))))

(defn make-sequencer [sequence]
  (let [state (atom (cycle sequence))]
    (fn []
      (let [current @state]
        (swap! state rest)
        (first current)))))

(defn make-excercise-word-generator-from-text [text]
  (make-sequencer (string/split text #"\s")))

(defn layout-demo-view [sample-text _layout]
  (let [generate-excercise-word (make-excercise-word-generator-from-text sample-text)
        state-atom (dependable-atom/atom {:typed-text ""
                                          :cocoa-key-code-down nil
                                          :target-word (generate-excercise-word)})
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
                                                                                                  (count (:typed-text @state-atom))))))))

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
                                                                  :target-word (generate-excercise-word)))))))}))))



(comment

  (view/start-view (fn []
                     (gui/black-background (layouts/center [#'layout-excercise-view text/finnish-statistics layout/qwerty]))))

  (view/start-view (fn []
                     (gui/black-background (layouts/center [#'layout-demo-view
                                                            (->> (-> "temp/text/the-hacker-crackdown.txt"
                                                                     slurp
                                                                     (subs 0 500)
                                                                     (string/replace #"\s+" " "))
                                                                 (map str)
                                                                 (map string/lower-case)
                                                                 (filter (conj (set text/english-characters)
                                                                               " "))
                                                                 (apply str))
                                                            layout/colemak-dh]))))
  )

(view/hard-refresh-view!)
