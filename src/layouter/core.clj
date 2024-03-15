(ns layouter.core
  (:require
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [com.stuartsierra.frequencies :as frequencies]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.component.button :as button]
   [fungl.component.text-area :as text-area]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [medley.core :as medley])
  (:import
   (java.util Random)))

;; KEYS

(def keyboard-keys [{:cocoa-key-code 0, :java-key-code 65, :finger 0, :row 1, :column 0}
                    {:cocoa-key-code 1, :java-key-code 83, :finger 1, :row 1, :column 1}
                    {:cocoa-key-code 2, :java-key-code 68, :finger 2, :row 1, :column 2}
                    {:cocoa-key-code 3, :java-key-code 70, :finger 3, :row 1, :column 3}
                    {:cocoa-key-code 4, :java-key-code 72, :finger 4, :row 1, :column 5}
                    {:cocoa-key-code 5, :java-key-code 71, :finger 3, :row 1, :column 4}
                    {:cocoa-key-code 6, :java-key-code 90, :finger 0, :row 2, :column 0}
                    {:cocoa-key-code 7, :java-key-code 88, :finger 1, :row 2, :column 1}
                    {:cocoa-key-code 8, :java-key-code 67, :finger 3, :row 2, :column 2}
                    {:cocoa-key-code 9, :java-key-code 86, :finger 3, :row 2, :column 3}
                    {:cocoa-key-code 11, :java-key-code 66, :finger 3, :row 2, :column 4}
                    {:cocoa-key-code 12, :java-key-code 81, :finger 0, :row 0, :column 0}
                    {:cocoa-key-code 13, :java-key-code 87, :finger 1, :row 0, :column 1}
                    {:cocoa-key-code 14, :java-key-code 69, :finger 2, :row 0, :column 2}
                    {:cocoa-key-code 15, :java-key-code 82, :finger 3, :row 0, :column 3}
                    {:cocoa-key-code 16, :java-key-code 89, :finger 4, :row 0, :column 5}
                    {:cocoa-key-code 17, :java-key-code 84, :finger 3, :row 0, :column 4}
                    {:cocoa-key-code 31, :java-key-code 79, :finger 6, :row 0, :column 8}
                    {:cocoa-key-code 32, :java-key-code 85, :finger 4, :row 0, :column 6}
                    {:cocoa-key-code 33, :java-key-code 16777445, :finger 7, :row 0, :column 10}
                    {:cocoa-key-code 34, :java-key-code 73, :finger 5, :row 0, :column 7}
                    {:cocoa-key-code 35, :java-key-code 80, :finger 7, :row 0, :column 9}
                    {:cocoa-key-code 37, :java-key-code 76, :finger 6, :row 1, :column 8}
                    {:cocoa-key-code 38, :java-key-code 74, :finger 4, :row 1, :column 6}
                    {:cocoa-key-code 39, :java-key-code 16777444, :finger 7, :row 1, :column 10}
                    {:cocoa-key-code 40, :java-key-code 75, :finger 5, :row 1, :column 7}
                    {:cocoa-key-code 41, :java-key-code 16777462, :finger 7, :row 1, :column 9}
                    {:cocoa-key-code 43, :java-key-code 44, :finger 5, :row 2, :column 7}
                    {:cocoa-key-code 44, :java-key-code 47, :finger 7, :row 2, :column 9}
                    {:cocoa-key-code 45, :java-key-code 78, :finger 3, :row 2, :column 5}
                    {:cocoa-key-code 46, :java-key-code 77, :finger 4, :row 2, :column 6}
                    {:cocoa-key-code 47, :java-key-code 46, :finger 6, :row 2, :column 8}])


(def cocoa-key-code-to-key (medley/index-by :cocoa-key-code keyboard-keys))

(def java-key-code-to-cocoa-key-code (medley/map-vals :cocoa-key-code
                                                      (medley/index-by :java-key-code keyboard-keys)))

(def cocoa-key-code-to-java-key-code (medley/map-vals :java-key-code
                                                      (medley/index-by :cocoa-key-code keyboard-keys)))





;; LAYOUTS


(defn layout-to-cocoa-key-code-to-character [layout]
  (medley/map-vals :character (medley/index-by :cocoa-key-code layout)))

(defn layout-to-character-to-cocoa-key-code [layout]
  (medley/map-vals :cocoa-key-code (medley/index-by :character layout)))

(defn layout-to-java-key-code-to-character [layout]
  (medley/map-keys
   (layout-to-cocoa-key-code-to-character layout))
  (medley/map-vals :character (medley/index-by :cocoa-key-code layout)))

(def qwerty #{{:character "a", :cocoa-key-code 0}
              {:character "s", :cocoa-key-code 1}
              {:character "d", :cocoa-key-code 2}
              {:character "f", :cocoa-key-code 3}
              {:character "h", :cocoa-key-code 4}
              {:character "g", :cocoa-key-code 5}
              {:character "z", :cocoa-key-code 6}
              {:character "x", :cocoa-key-code 7}
              {:character "c", :cocoa-key-code 8}
              {:character "v", :cocoa-key-code 9}
              {:character "b", :cocoa-key-code 11}
              {:character "q", :cocoa-key-code 12}
              {:character "w", :cocoa-key-code 13}
              {:character "e", :cocoa-key-code 14}
              {:character "r", :cocoa-key-code 15}
              {:character "y", :cocoa-key-code 16}
              {:character "t", :cocoa-key-code 17}
              {:character "o", :cocoa-key-code 31}
              {:character "u", :cocoa-key-code 32}
              {:character "å", :cocoa-key-code 33}
              {:character "i", :cocoa-key-code 34}
              {:character "p", :cocoa-key-code 35}
              {:character "l", :cocoa-key-code 37}
              {:character "j", :cocoa-key-code 38}
              {:character "ä", :cocoa-key-code 39}
              {:character "k", :cocoa-key-code 40}
              {:character "ö", :cocoa-key-code 41}
              {:character ",", :cocoa-key-code 43}
              {:character "-", :cocoa-key-code 44}
              {:character "n", :cocoa-key-code 45}
              {:character "m", :cocoa-key-code 46}
              {:character ".", :cocoa-key-code 47}})

(def layout-characters (into #{} (map :character qwerty)))


(defn layout-from-qwerty [new-layout-character-mapping-to-qwerty]
  (let [character-to-cocoa-key-code-in-qwerty (layout-to-character-to-cocoa-key-code qwerty)]
    (into #{}
          (for [[new-layout-character qwerty-character] new-layout-character-mapping-to-qwerty]
            {:character new-layout-character
             :cocoa-key-code (character-to-cocoa-key-code-in-qwerty qwerty-character)}))))

(deftest test-layout-from-qwerty
  (is (= #{{:character "a", :cocoa-key-code 3}
           {:character "b", :cocoa-key-code 38}}
         (layout-from-qwerty {"a" "f"
                              "b" "j"}))))


(defn layout-from-character-rows [row-1 row-2 row-2]
  ;; (let [character-to-cocoa-key-code-in-qwerty (layout-to-character-to-cocoa-key-code qwerty)]
  ;;   (into #{}
  ;;         (for [[new-layout-character qwerty-character] new-layout-character-mapping-to-qwerty]
  ;;           {:character new-layout-character
  ;;            :cocoa-key-code (character-to-cocoa-key-code-in-qwerty qwerty-character)})))
  )



;; DIGRAMS

(defn add-word-digram-distribution [probabilities word]
  (if (= 1 (count word))
    (update probabilities
            #{word}
            (fnil inc 0))
    (reduce (fn [probabilities [c1 c2]]
              (update probabilities
                      (if (= c1 c2)
                        #{c1}
                        #{c1 c2})
                      (fnil inc 0)))
            probabilities
            (map #(map str %)
                 (partition 2 1
                            word)))))

(deftest test-add-word-digram-distribution
  (is (= {#{"a" "b"} 2, #{"b" "c"} 1, #{"a" "c"} 1}
         (add-word-digram-distribution {} "abcab")))

  (is (= {#{"a"} 1}
         (add-word-digram-distribution {} "a"))))

(defn extract-words [text]
  (remove (fn [word]
            (= 1 (count word)))
          (remove empty? (string/split text #"\s+"))))

(deftest test-extract-words
  (is (= '("abc" "abc" "abc")
         (extract-words "abc abc  \nabc a"))))

(defn text-digram-distribution [text]
  (reduce add-word-digram-distribution
          {}
          (extract-words text)))

(deftest test-text-digram-distribution
  (is (= {#{"a" "b"} 1, #{"d" "c"} 1}
         (text-digram-distribution "ab cd")))

  (is (= {#{"a" "b"} 1, #{"d" "c"} 1}
         (text-digram-distribution "ab\ncd")))

  (is (= {#{"e" "h"} 1, #{"e" "l"} 1, #{"l"} 1, #{"l" "o"} 1}
         (text-digram-distribution "hello"))))




;; RATING

(def finger-hand {0 0
                  1 0
                  2 0
                  3 0
                  4 1
                  5 1
                  6 1
                  7 1})

(def finger-rating {3 0
                    4 0
                    2 -1
                    5 -1
                    1 -2
                    6 -2
                    0 -4
                    7 -4})

(def home-position-cocoa-key-codes #{0 1 2 3 38 40 37 41})

(defn rate-cocoa-key-code [cocoa-key-code]
  (+ (if (contains? home-position-cocoa-key-codes cocoa-key-code)
       0
       -2)
     (* 0.5 (finger-rating (:finger (cocoa-key-code-to-key cocoa-key-code))))))


(defn rate-key-pair [key-pair]
  (+ (cond (= 2 (count (map finger-hand (map :finger key-pair))))
           0

           (and (= 1 (count (map :finger key-pair)))
                (= #{0 2} (set (map :row key-pair))))
           -3

           (and (= 1 (count (map :finger key-pair)))
                (or (= #{0 1} (set (map :row key-pair)))
                    (= #{1 2} (set (map :row key-pair)))))
           -2

           :else
           -1)
     (* 2 (reduce + (map rate-cocoa-key-code (map :cocoa-key-code key-pair))))))

(defn rate-layout [text-digram-distribution layout]
  (let [character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)]
    (reduce +
            (for [[[character-1 character-2] digram-count] (map (fn [[character-set digram-count]]
                                                                  [(vec (sort character-set)) digram-count])
                                                                text-digram-distribution)]
              (* digram-count
                 (rate-key-pair [(cocoa-key-code-to-key (character-to-cocoa-key-code character-1))
                                 (cocoa-key-code-to-key (character-to-cocoa-key-code (or character-2
                                                                                         character-1)))]))))))

(deftest test-rate-layout
  (is (= -28.0
         (rate-layout (text-digram-distribution "hello")
                      qwerty)))

  (is (= -5.0
         (rate-layout (text-digram-distribution "hello")
                      (layout-from-qwerty {"h" "j"
                                           "e" "f"
                                           "l" "k"
                                           "o" "d"})))))


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
                   space-cocoa-key-code " "))
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
      (key-log-to-string (* 1 second-in-microseconds))))


(comment

  (string/join  " " (extract-words (key-log-to-string-from-file key-log-file-path)))
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




;; GENETIC ALGORITHM

(defn mutate-layout [layout]
  (let [layout-vector (vec layout)
        mapping-1 (rand-nth layout-vector)
        mapping-2 (rand-nth (remove #{mapping-1}
                                    layout-vector))]
    (-> layout
        (disj mapping-1)
        (disj mapping-2)
        (conj {:character (:character mapping-1)
               :cocoa-key-code (:cocoa-key-code mapping-2)})
        (conj {:character (:character mapping-2)
               :cocoa-key-code (:cocoa-key-code mapping-1)}))))

(deftest test-mutate-layout
  (is (= #{{:character "b", :cocoa-key-code 0}
           {:character "a", :cocoa-key-code 1}}
         (mutate-layout #{{:character "a" :cocoa-key-code 0}
                          {:character "b" :cocoa-key-code 1}}))))

(comment
  (sort-by :cocoa-key-code
           (mutate-layout #{{:character "a" :cocoa-key-code 0}
                            {:character "b" :cocoa-key-code 1}
                            {:character "c" :cocoa-key-code 2}
                            {:character "d" :cocoa-key-code 3}})))

(defn random-layout []
  (loop [remaining-cocoa-key-codes (map :cocoa-key-code qwerty)
         remaining-characters (map :character qwerty)
         layout #{}]
    (if (empty? remaining-characters)
      layout
      (let [character (first remaining-characters)
            cocoa-key-code (rand-nth remaining-cocoa-key-codes)]
        (recur (remove #{cocoa-key-code} remaining-cocoa-key-codes)
               (rest remaining-characters)
               (conj layout {:character character
                             :cocoa-key-code cocoa-key-code}))))))

(def ^:dynamic random (Random.))

(defn crossbreed-layouts [layout-1 layout-2]
  (loop [cocoa-key-code-to-character-1 (layout-to-cocoa-key-code-to-character layout-1)
         cocoa-key-code-to-character-2 (layout-to-cocoa-key-code-to-character layout-2)
         new-layout #{}
         cocoa-key-codes (map :cocoa-key-code layout-1)]
    (if (empty? cocoa-key-codes)
      new-layout
      (let [cocoa-key-code (first cocoa-key-codes)
            character (if (< 0.5 (.nextDouble random))
                        (or (cocoa-key-code-to-character-1 cocoa-key-code)
                            (cocoa-key-code-to-character-2 cocoa-key-code)
                            (first (vals cocoa-key-code-to-character-1)))
                        (or (cocoa-key-code-to-character-2 cocoa-key-code)
                            (cocoa-key-code-to-character-1 cocoa-key-code)
                            (first (vals cocoa-key-code-to-character-2))))]
        (recur (medley/remove-vals #{character}
                                   cocoa-key-code-to-character-1)
               (medley/remove-vals #{character}
                                   cocoa-key-code-to-character-2)
               (conj new-layout
                     {:cocoa-key-code cocoa-key-code
                      :character character})
               (rest cocoa-key-codes))))))

(deftest test-crossbreed-layouts
  (is (= #{{:cocoa-key-code 0, :character "a"}
           {:cocoa-key-code 1, :character "b"}}
         (binding [random (Random. 1)]
           (crossbreed-layouts #{{:character "a" :cocoa-key-code 0}
                                 {:character "b" :cocoa-key-code 1}}
                               #{{:character "a" :cocoa-key-code 1}
                                 {:character "b" :cocoa-key-code 0}}))))

  (is (= #{{:cocoa-key-code 3, :character "c"}
           {:cocoa-key-code 0, :character "a"}
           {:cocoa-key-code 1, :character "b"}}
         (binding [random (Random. 1)]
           (crossbreed-layouts #{{:character "a" :cocoa-key-code 0}
                                 {:character "b" :cocoa-key-code 1}
                                 {:character "c" :cocoa-key-code 3}}
                               #{{:character "a" :cocoa-key-code 3}
                                 {:character "b" :cocoa-key-code 0}
                                 {:character "c" :cocoa-key-code 1}})))))

(defn weighted-random [distribution]
  (let [max-sum (* (reduce + (map second distribution))
                   (rand))]
    (loop [sum 0
           distribution distribution]
      (let [propability (first distribution)
            sum (+ sum (second propability))]
        (if (<= (abs sum)
                (abs max-sum))
          (recur sum
                 (rest distribution))
          (first propability))))))


(comment
  ((apply comp (repeat 3 mutate-layout))
   #{{:character "a" :cocoa-key-code 3}
     {:character "b" :cocoa-key-code 0}
     {:character "c" :cocoa-key-code 1}})
  ) ;; TODO: remove me

(def continue-optimization?-atom (atom true))



(defn optimize-layout [text initial-layout mutation-count generation-count on-ready]
  (let [text-digram-distribution (text-digram-distribution text)
        generation-size 50
        ;;        maximum-generations 500

        mutate-layout (apply comp (repeat mutation-count mutate-layout))
        initial-layouts (concat [initial-layout]
                                (map mutate-layout
                                     (repeat (dec (/ generation-size
                                                     2))
                                             initial-layout))
                                (repeatedly (/ generation-size
                                               2)
                                            random-layout))]
    (loop [generation-number 0
           ratings (for [layout initial-layouts]
                     [layout (rate-layout text-digram-distribution layout)])]

      (let [crossbreeded-ratings (for [layout (map mutate-layout
                                                   (repeatedly generation-size
                                                               #(crossbreed-layouts (weighted-random ratings)
                                                                                    (weighted-random ratings))))]
                                   [layout (rate-layout text-digram-distribution layout)])
            next-ratings (take-last generation-size
                                    (sort-by second
                                             (concat ratings
                                                     crossbreeded-ratings
                                                     (for [layout (repeatedly 5 random-layout)]
                                                       [layout (rate-layout text-digram-distribution
                                                                            layout)]))))]

        (when (= 0 (mod generation-number 50))
          (prn) ;; TODO: remove me
          (prn 'generation-number generation-number) ;; TODO: remove me
          (prn 'ratings (take 10 (reverse (sort (map second next-ratings)))))) ;; TODO: remove me

        (if (< generation-number
               generation-count)
          (recur (inc generation-number)
                 next-ratings)
          (on-ready (first (last (sort-by second ratings)))))))))

(defn filter-target-text [text]
  (apply str (filter (conj layout-characters " ")
                     (map str (string/lower-case text)))))

(defn filter-target-text-without-space [text]
  (apply str
         (filter layout-characters
                 (map str (string/lower-case text)))))

(def target-text #_"hello world"
  (filter-target-text (str (subs (slurp "text/kirjoja-ja-kirjailijoita.txt")
                                 0 300000)
                           (subs (slurp "text/the-hacker-crackdown.txt")
                                 0 300000))
                      #_(slurp "text/kirjoja-ja-kirjailijoita.txt"
                               #_"text/ga-fi.txt"
                               #_"text/ga.txt")))

(comment
  (count (slurp "text/kirjoja-ja-kirjailijoita.txt"))
  ;; => 324920
  (count (slurp "text/the-hacker-crackdown.txt"))
  ;; => 663795
  ) ;; TODO: remove me

(defonce optimized-layout qwerty)

(defn normalize-distribution [digram-distribution]
  (let [total-count (reduce + (vals digram-distribution))]
    (medley/map-vals (fn [count]
                       (double (/ count total-count)))
                     digram-distribution)))

(defn normalized-digram-distribution [text]
  (->> text
       filter-target-text
       text-digram-distribution
       normalize-distribution
       ;; (sort-by second)
       ;; reverse
       ))

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

(defn normalized-character-distribution [text]
  (->> text
       filter-target-text-without-space
       frequencies
       normalize-distribution
       (medley/map-keys str)))

(deftest test-normalized-character-distribution
  (is (= {"h" 0.2, "e" 0.2, "l" 0.4, "o" 0.2}
         (normalized-character-distribution "hello"))))


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
  (compare-distributions [(normalized-character-distribution (str (slurp "text/kirjoja-ja-kirjailijoita.txt")
                                                                  (slurp "text/the-hacker-crackdown.txt")))
                          (normalized-character-distribution (key-log-to-string key-log-file-path))]))

(defn compare-digram-distributions []
  (compare-distributions [ ;; (normalized-digram-distribution (slurp "text/the-hacker-crackdown.txt"))
                          ;; (normalized-digram-distribution (slurp "text/tietokoneet.txt"))
                          ;; (normalized-digram-distribution (slurp "text/the-hacker-crackdown.txt"))
                          ;; (normalized-digram-distribution (remove-spaces (slurp "text/the-hacker-crackdown.txt")))
                          (normalized-digram-distribution (str (slurp "text/kirjoja-ja-kirjailijoita.txt")
                                                               (slurp "text/the-hacker-crackdown.txt")))
                          (normalized-digram-distribution (key-log-to-string key-log-file-path))]))
(comment
  (compare-distributions [(normalized-character-distribution "abccc")
                          (normalized-character-distribution "abdd")])
  (compare-character-cistributions)
  (compare-digram-distributions)
  (normalized-character-distribution "hello")
  (def english-digram-distribution (normalized-digram-distribution (slurp "text/the-hacker-crackdown.txt")))

  (def hacker-distribution (normalized-digram-distribution (slurp "text/the-hacker-crackdown.txt")))
  (def corncob-distribution (normalized-digram-distribution (slurp "text/corncob_lowercase.txt")))

  ;; (let [first-distribution hacker-distribution
  ;;       second-distribution (normalized-digram-distribution (slurp "text/ga.txt"))

  ;;       first-positions (distribution-positions first-distribution)
  ;;       second-positions (distribution-positions second-distribution)

  ;;       digram-distributions [(normalized-digram-distribution (slurp "text/the-hacker-crackdown.txt"))
  ;;                             (normalized-digram-distribution (slurp "text/tietokoneet.txt"))]
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





  (count (normalized-digram-distribution (slurp "text/corncob_lowercase.txt")))
  ;; => 342
  (count english-digram-distribution)

  (def optimized-layout qwerty)
  (optimize-layout (filter-target-text target-text)
                   optimized-layout
                   1
                   100
                   (fn [new-layout]
                     (def optimized-layout new-layout)))

  (count (normalized-digram-distribution (filter-target-text (slurp "text/kirjoja-ja-kirjailijoita.txt"))))
  ;; => 413

  (doseq [layout-file-name ["qwerty.edn"
                            "optimized-layout-for-finnish-and-english.edn"
                            "optimized-layout-for-finnish.edn"
                            "optimized-layout-for-english.edn"]]
    (println)
    (println layout-file-name)
    (let [layout (edn/read-string (slurp layout-file-name))]
      (doseq [target-text-file-name ["text/the-hacker-crackdown.txt"
                                     "text/kirjoja-ja-kirjailijoita.txt"]]
        (println target-text-file-name
                 (rate-layout (take 300 (reverse (sort-by second (normalized-digram-distribution (slurp target-text-file-name)))))
                              layout)))))

  ;; qwerty.edn
  ;; text/the-hacker-crackdown.txt -8.200177457130849
  ;; text/kirjoja-ja-kirjailijoita.txt -8.067552298454757

  ;; optimized-layout-for-finnish-and-english.edn
  ;; text/the-hacker-crackdown.txt -5.41650933789515
  ;; text/kirjoja-ja-kirjailijoita.txt -4.781927577581428

  ;; optimized-layout-for-finnish.edn
  ;; text/the-hacker-crackdown.txt -5.869223441281065
  ;; text/kirjoja-ja-kirjailijoita.txt -4.6758328653527315

  ;; optimized-layout-for-english.edn
  ;; text/the-hacker-crackdown.txt -5.3888708559434235
  ;; text/kirjoja-ja-kirjailijoita.txt -5.800231798034474

  )



;; UI

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

(def font (font/create-by-name "CourierNewPSMT" 40))

(defn text [string & [{:keys [font color] :or {font font
                                               color (:text-color theme)}}]]
  (text-area/text (str string)
                  color
                  font))


(defonce event-channel-atom (atom nil))

(defn start-view [view]
  (reset! event-channel-atom
          (application/start-application view
                                         :on-exit #(reset! event-channel-atom nil))))

(defn row-view [characters character-color]
  (layouts/horizontally-2 {:margin 1}
                          (for [character characters]
                            (box (text character)
                                 {:fill-color (or (character-color character)
                                                  [70 70 70 255])
                                  :padding 10}))))

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
                                                    (abs (int (rate-cocoa-key-code (character-to-cocoa-key-code character)))))]
                                   (text key-rating
                                         {:color (if (not (or (= 0 key-rating)
                                                              (= 1 key-rating)))
                                                   [200 100 100 255]
                                                   (:text-color theme))}))
                                 (text (when (and (contains? layout-characters character)
                                                  (contains? layout-characters next-character))
                                         (str (abs (int (/ (rate-key-pair [(cocoa-key-code-to-key (character-to-cocoa-key-code character))
                                                                           (cocoa-key-code-to-key (character-to-cocoa-key-code next-character))])
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


(defn keyboard-view [cocoa-key-code-to-character character-color]
  (layouts/vertically-2 {:margin 10}
                        (layouts/vertically-2 {:margin 1}
                                              (layouts/horizontally-2 {:margin 10}
                                                                      (row-view (map cocoa-key-code-to-character [12 13 14 15 17])
                                                                                character-color)
                                                                      (row-view (map cocoa-key-code-to-character [16 32 34 31 35 33])
                                                                                character-color))
                                              (layouts/with-margins 0 0 0 10
                                                (layouts/horizontally-2 {:margin 10}
                                                                        (row-view (map cocoa-key-code-to-character [0 1 2 3 5])
                                                                                  character-color)
                                                                        (row-view (map cocoa-key-code-to-character [4 38 40 37 41 39])
                                                                                  character-color)))
                                              (layouts/with-margins 0 0 0 30
                                                (layouts/horizontally-2 {:margin 10}
                                                                        (row-view (map cocoa-key-code-to-character [6 7 8 9 11])
                                                                                  character-color)
                                                                        (row-view (map cocoa-key-code-to-character [45 46 43 47 44])
                                                                                  character-color))))))

(defn digram-test-view [layout target-text _highlighted-characters]
  (let [state-atom (dependable-atom/atom {:highlighted-characters #{}})
        cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)
        character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)
        digram-distribution (normalized-digram-distribution (filter-target-text target-text))]
    (fn [layout _target-text highlighted-characters]
      (let [character-color (into {}
                                  (concat (for [highlighted-character highlighted-characters]
                                            [highlighted-character [70 100 70 255]])
                                          (for [highlighted-character (:highlighted-characters @state-atom)]
                                            [highlighted-character [100 150 70 255]])))]
        (layouts/vertically-2 {:margin 10}
                              [keyboard-view cocoa-key-code-to-character
                               character-color]
                              (text (format "%.3f"
                                            (rate-layout digram-distribution
                                                         layout)))
                              (digram-view digram-distribution
                                           character-to-cocoa-key-code
                                           (fn [digram]
                                             (swap! state-atom assoc :highlighted-characters (or digram
                                                                                                 #{})))))))))


(defn text-test-view [layout _text]
  (let [state-atom (dependable-atom/atom {:highlighted-character nil})
        layout-rating (rate-layout (normalized-digram-distribution text)
                                   layout)]
    (fn [layout test-text]
      (let [cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)
            character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)]
        (layouts/vertically-2 {:margin 10}
                              [keyboard-view
                               cocoa-key-code-to-character
                               {(:highlighted-character @state-atom) [100 150 70 255]}]
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
                                                                                    (swap! state-atom assoc :highlighted-character character-under-mouse)))
                                                                         (partition-all 2 1 (map str test-text))))))))))
(comment
  (let [test-text "kotivara vetää myynnistä lisää makkaroita"]
    (start-view (fn []
                  (layouts/vertically-2 {:margin 10}
                                        ;; [#'text-test-view
                                        ;;  qwerty
                                        ;;  test-text]
                                        [#'text-test-view
                                         (edn/read-string (slurp "optimized-layout-for-finnish-and-english.edn"))
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
;;             digram-distribution (text-digram-distribution (filter-target-text target-text))
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


(defn start-optimization [state-atom target-text]
  (.start (Thread. (fn []
                     (optimize-layout (filter-target-text target-text)
                                      (last (:layouts @state-atom))
                                      (rand-int 5)
                                      100
                                      (fn [new-layout]
                                        (when (not (= new-layout
                                                      (last (:layouts @state-atom))))
                                          (def optimized-layout new-layout)
                                          (swap! state-atom
                                                 update
                                                 :layouts (fn [layouts]
                                                            (take-last 4
                                                                       (concat layouts
                                                                               [new-layout])))))
                                        (async/>!! @event-channel-atom
                                                   {:type :foo})
                                        (when (:optimize? @state-atom)
                                          (start-optimization state-atom
                                                              target-text))))))))

(defn optimization-view [_target-text]
  (let [state-atom (dependable-atom/atom {:layouts [(random-layout)
                                                    #_optimized-layout]
                                          :optimize? false})]
    (fn [target-text]
      (let [state @state-atom]
        (layouts/vertically-2 {:margin 10}
                              (button/button (if (:optimize? state)
                                               "stop optimize"
                                               "start optimize")
                                             (fn []
                                               (swap! state-atom update :optimize? not)
                                               (when (:optimize? @state-atom)
                                                 (start-optimization state-atom
                                                                     target-text))))
                              (layouts/horizontally-2 {:margin 10}
                                                      (concat [[digram-test-view
                                                                (first (:layouts state))
                                                                target-text
                                                                #{}]]
                                                              (for [[previous-layout layout] (partition 2 1 (:layouts state))]
                                                                [digram-test-view
                                                                 layout
                                                                 target-text
                                                                 (set/difference layout-characters
                                                                                 (set (map :character (set/intersection previous-layout
                                                                                                                        layout))))])))
                              [#'text-test-view
                               (last (:layouts state))
                               target-text])))))

(comment
  (start-view (fn []
                [#'optimization-view "kotivara vetää myynnistä lisää makkaroita"]))
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
    (let [texts [(str (subs (slurp "text/kirjoja-ja-kirjailijoita.txt")
                            0 300000)
                      (subs (slurp "text/the-hacker-crackdown.txt")
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
  (reset! event-channel-atom
          (application/start-application
           #'optimization-view
           ;; #'digram-distribution-comparison-view
           :on-exit #(reset! event-channel-atom nil))))

(when @event-channel-atom
  (async/>!! @event-channel-atom
             {:type :redraw}))

(comment

  (spit "optimized-layout-for-finnish-and-english.edn" (pr-str optimized-layout))
  (spit "qwerty.edn" (pr-str qwerty))
  (def optimized-layout (edn/read-string (slurp "optimized-layout.edn")))
  ) ;; TODO: remove me
