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

;; KEYS

(def key-class-effort {:home 0
                       :easy-index 0.15
                       :regular 0.25
                       :sideways 0.55
                       :middle 1})

(def keyboard-keys [{:cocoa-key-code 0  :java-key-code 65       :finger 0 :class :home       :row 1 :column 0  :qwerty-character "a" :disabled? false}
                    {:cocoa-key-code 1  :java-key-code 83       :finger 1 :class :home       :row 1 :column 1  :qwerty-character "s" :disabled? false}
                    {:cocoa-key-code 2  :java-key-code 68       :finger 2 :class :home       :row 1 :column 2  :qwerty-character "d" :disabled? false}
                    {:cocoa-key-code 3  :java-key-code 70       :finger 3 :class :home       :row 1 :column 3  :qwerty-character "f" :disabled? false}
                    {:cocoa-key-code 4  :java-key-code 72       :finger 4 :class :regular    :row 1 :column 5  :qwerty-character "h" :disabled? false}
                    {:cocoa-key-code 5  :java-key-code 71       :finger 3 :class :regular    :row 1 :column 4  :qwerty-character "g" :disabled? false}
                    {:cocoa-key-code 6  :java-key-code 90       :finger 1 :class :regular    :row 2 :column 1  :qwerty-character "z" :disabled? false}
                    {:cocoa-key-code 7  :java-key-code 88       :finger 2 :class :regular    :row 2 :column 2  :qwerty-character "x" :disabled? false}
                    {:cocoa-key-code 8  :java-key-code 67       :finger 3 :class :easy-index :row 2 :column 3  :qwerty-character "c" :disabled? false}
                    {:cocoa-key-code 9  :java-key-code 86       :finger 3 :class :sideways   :row 2 :column 4  :qwerty-character "v" :disabled? false}
                    {:cocoa-key-code 11 :java-key-code 66       :finger 3 :class :middle     :row 2 :column 4  :qwerty-character "b" :disabled? false}
                    {:cocoa-key-code 12 :java-key-code 81       :finger 0 :class :regular    :row 0 :column 0  :qwerty-character "q" :disabled? false}
                    {:cocoa-key-code 13 :java-key-code 87       :finger 1 :class :sideways   :row 0 :column 0  :qwerty-character "w" :disabled? false}
                    {:cocoa-key-code 14 :java-key-code 69       :finger 1 :class :regular    :row 0 :column 1  :qwerty-character "e" :disabled? false}
                    {:cocoa-key-code 15 :java-key-code 82       :finger 2 :class :regular    :row 0 :column 2  :qwerty-character "r" :disabled? false}
                    {:cocoa-key-code 16 :java-key-code 89       :finger 4 :class :middle     :row 0 :column 5  :qwerty-character "y" :disabled? false}
                    {:cocoa-key-code 17 :java-key-code 84       :finger 3 :class :regular    :row 0 :column 3  :qwerty-character "t" :disabled? false}
                    {:cocoa-key-code 31 :java-key-code 79       :finger 6 :class :regular    :row 0 :column 8  :qwerty-character "o" :disabled? false}
                    {:cocoa-key-code 32 :java-key-code 85       :finger 4 :class :regular    :row 0 :column 6  :qwerty-character "u" :disabled? false}
                    {:cocoa-key-code 33 :java-key-code 91       :finger 7 :class :regular    :row 0 :column 10 :qwerty-character "å" :disabled? false}
                    {:cocoa-key-code 34 :java-key-code 73       :finger 5 :class :regular    :row 0 :column 7  :qwerty-character "i" :disabled? false}
                    {:cocoa-key-code 35 :java-key-code 80       :finger 7 :class :regular    :row 0 :column 9  :qwerty-character "p" :disabled? false}
                    {:cocoa-key-code 37 :java-key-code 76       :finger 6 :class :home       :row 1 :column 8  :qwerty-character "l" :disabled? false}
                    {:cocoa-key-code 38 :java-key-code 74       :finger 4 :class :home       :row 1 :column 6  :qwerty-character "j" :disabled? false}
                    {:cocoa-key-code 39 :java-key-code 222      :finger 7 :class :regular    :row 1 :column 10 :qwerty-character "ä" :disabled? false}
                    {:cocoa-key-code 40 :java-key-code 75       :finger 5 :class :home       :row 1 :column 7  :qwerty-character "k" :disabled? false}
                    {:cocoa-key-code 41 :java-key-code 59       :finger 7 :class :home       :row 1 :column 9  :qwerty-character "ö" :disabled? false}
                    {:cocoa-key-code 43 :java-key-code 44       :finger 5 :class :regular    :row 2 :column 7  :qwerty-character "," :disabled? true}
                    {:cocoa-key-code 44 :java-key-code 47       :finger 7 :class :regular    :row 2 :column 9  :qwerty-character "-" :disabled? true}
                    {:cocoa-key-code 45 :java-key-code 78       :finger 3 :class :sideways   :row 2 :column 5  :qwerty-character "n" :disabled? false}
                    {:cocoa-key-code 46 :java-key-code 77       :finger 4 :class :easy-index :row 2 :column 6  :qwerty-character "m" :disabled? false}
                    {:cocoa-key-code 47 :java-key-code 46       :finger 6 :class :regular    :row 2 :column 8  :qwerty-character "." :disabled? true}
                    {:cocoa-key-code 50 :java-key-code 192      :finger 0 :class :regular    :row 2 :column 0  :qwerty-character "<" :disabled? true}])


(def cocoa-key-code-to-key (medley/index-by :cocoa-key-code keyboard-keys))

(def qwerty-character-to-key (medley/index-by :qwerty-character keyboard-keys))

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
              {:character ".", :cocoa-key-code 47}
              {:character "<", :cocoa-key-code 50}})

(def colemak-dh #{{:cocoa-key-code 0, :qwerty-character "a", :character "a"}
                  {:cocoa-key-code 1, :qwerty-character "s", :character "r"}
                  {:cocoa-key-code 2, :qwerty-character "d", :character "s"}
                  {:cocoa-key-code 3, :qwerty-character "f", :character "t"}
                  {:cocoa-key-code 4, :qwerty-character "h", :character "m"}
                  {:cocoa-key-code 5, :qwerty-character "g", :character "g"}
                  {:cocoa-key-code 6, :qwerty-character "z", :character "x"}
                  {:cocoa-key-code 7, :qwerty-character "x", :character "c"}
                  {:cocoa-key-code 8, :qwerty-character "c", :character "d"}
                  {:cocoa-key-code 9, :qwerty-character "v", :character "v"}
                  {:cocoa-key-code 11, :qwerty-character "b", :character ""}
                  {:cocoa-key-code 12, :qwerty-character "q", :character "q"}
                  {:cocoa-key-code 13, :qwerty-character "w", :character "w"}
                  {:cocoa-key-code 14, :qwerty-character "e", :character "f"}
                  {:cocoa-key-code 15, :qwerty-character "r", :character "p"}
                  {:cocoa-key-code 16, :qwerty-character "y", :character "j"}
                  {:cocoa-key-code 17, :qwerty-character "t", :character "b"}
                  {:cocoa-key-code 31, :qwerty-character "o", :character "y"}
                  {:cocoa-key-code 32, :qwerty-character "u", :character "l"}
                  {:cocoa-key-code 33, :qwerty-character "å", :character "å"}
                  {:cocoa-key-code 34, :qwerty-character "i", :character "u"}
                  {:cocoa-key-code 35, :qwerty-character "p", :character "ö"}
                  {:cocoa-key-code 37, :qwerty-character "l", :character "i"}
                  {:cocoa-key-code 38, :qwerty-character "j", :character "n"}
                  {:cocoa-key-code 39, :qwerty-character "ä", :character "ä"}
                  {:cocoa-key-code 40, :qwerty-character "k", :character "e"}
                  {:cocoa-key-code 41, :qwerty-character "ö", :character "o"}
                  {:cocoa-key-code 43, :qwerty-character ",", :character ","}
                  {:cocoa-key-code 44, :qwerty-character "-", :character "-"}
                  {:cocoa-key-code 45, :qwerty-character "n", :character "k"}
                  {:cocoa-key-code 46, :qwerty-character "m", :character "h"}
                  {:cocoa-key-code 47, :qwerty-character ".", :character "."}
                  {:cocoa-key-code 50, :qwerty-character "<", :character "z"}})

(def dvorak #{{:cocoa-key-code 0, :qwerty-character "a", :character "a"}
              {:cocoa-key-code 1, :qwerty-character "s", :character "o"}
              {:cocoa-key-code 2, :qwerty-character "d", :character "e"}
              {:cocoa-key-code 3, :qwerty-character "f", :character "u"}
              {:cocoa-key-code 4, :qwerty-character "h", :character "d"}
              {:cocoa-key-code 5, :qwerty-character "g", :character "i"}
              {:cocoa-key-code 6, :qwerty-character "z", :character "."}
              {:cocoa-key-code 7, :qwerty-character "x", :character "q"}
              {:cocoa-key-code 8, :qwerty-character "c", :character "j"}
              {:cocoa-key-code 9, :qwerty-character "v", :character "k"}
              {:cocoa-key-code 11, :qwerty-character "b", :character "x"}
              {:cocoa-key-code 12, :qwerty-character "q", :character "å"}
              {:cocoa-key-code 13, :qwerty-character "w", :character "ä"}
              {:cocoa-key-code 14, :qwerty-character "e", :character "ö"}
              {:cocoa-key-code 15, :qwerty-character "r", :character "p"}
              {:cocoa-key-code 16, :qwerty-character "y", :character "f"}
              {:cocoa-key-code 17, :qwerty-character "t", :character "y"}
              {:cocoa-key-code 31, :qwerty-character "o", :character "r"}
              {:cocoa-key-code 32, :qwerty-character "u", :character "g"}
              {:cocoa-key-code 33, :qwerty-character "å", :character ","}
              {:cocoa-key-code 34, :qwerty-character "i", :character "c"}
              {:cocoa-key-code 35, :qwerty-character "p", :character "l"}
              {:cocoa-key-code 37, :qwerty-character "l", :character "n"}
              {:cocoa-key-code 38, :qwerty-character "j", :character "h"}
              {:cocoa-key-code 39, :qwerty-character "ä", :character "-"}
              {:cocoa-key-code 40, :qwerty-character "k", :character "t"}
              {:cocoa-key-code 41, :qwerty-character "ö", :character "s"}
              {:cocoa-key-code 43, :qwerty-character ",", :character "w"}
              {:cocoa-key-code 44, :qwerty-character "-", :character "z"}
              {:cocoa-key-code 45, :qwerty-character "n", :character "b"}
              {:cocoa-key-code 46, :qwerty-character "m", :character "m"}
              {:cocoa-key-code 47, :qwerty-character ".", :character "v"}
              {:cocoa-key-code 50, :qwerty-character "<", :character "<"}})

(def english-characters ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                         "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(def finnish-characters (concat english-characters
                                ["ä" "å" "ö"]))

(def disabled-layout-characters #{"," "." "-" "<" "ö" "ä" "å"})

(def layout-characters (set/difference (into #{} (map :character qwerty))
                                       disabled-layout-characters))


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

(defn add-word-to-n-gram-distribution [n probabilities word]
  (if (> n (count word))
    probabilities
    (reduce (fn [probabilities n-gram]
              (update probabilities
                      n-gram
                      (fnil inc 0)))
            probabilities
            (map #(map str %)
                 (partition n 1 word)))))

(deftest test-add-word-to--n-gram-distribution
  (is (= {["a" "b"] 2, ["b" "b"] 1, ["b" "c"] 1, ["c" "a"] 1}
         (add-word-to-n-gram-distribution 2 {} "abbcab")))

  (is (= {}
         (add-word-to-n-gram-distribution 2 {} "a"))))

(defn extract-words [text]
  (remove (fn [word]
            (= 1 (count word)))
          (remove empty? (string/split text #"\s+"))))

(deftest test-extract-words
  (is (= '("abc" "abc" "abc")
         (extract-words "abc abc  \nabc a"))))

(defn n-gram-distribution [n text]
  (reduce (partial add-word-to-n-gram-distribution n)
          {}
          (extract-words text)))

(deftest test-digram-distribution
  (is (= {["a" "b"] 1, ["c" "d"] 1}
         (n-gram-distribution 2 "ab cd")))

  (is (= {["a" "b"] 1, ["c" "d"] 1}
         (n-gram-distribution 2 "ab\ncd")))

  (is (= {["h" "e"] 1, ["e" "l"] 1, ["l" "l"] 1, ["l" "o"] 1}
         (n-gram-distribution 2 "hello")))

  (is (= '{("h" "e" "l") 1, ("e" "l" "l") 1, ("l" "l" "o") 1}
         (n-gram-distribution 3 "hello"))))



(defn character-distribution [text]
  (medley/map-keys str
                   (frequencies text)))

(deftest test-character-distribution
  (is (= {"h" 1, "e" 1, "l" 2, "o" 1}
         (character-distribution "hello"))))

(defn normalize-distribution [distribution]
  (let [total-count (reduce + (vals distribution))]
    (medley/map-vals (fn [count]
                       (double (/ count total-count)))
                     distribution)))

(defn select-probability-mass [maximum-mass distribution]
  (into {}
        (loop [propabilities (->> distribution
                                  (sort-by second)
                                  (reverse))
               selected-propabilities []
               total-propability 0]
          (if (empty? propabilities)
            selected-propabilities
            (let [propability (first propabilities)]
              (if (< maximum-mass
                     (+ total-propability
                        (second propability)))
                selected-propabilities
                (recur (rest propabilities)
                       (conj selected-propabilities
                             propability)
                       (+ total-propability
                          (second propability)))))))))

(deftest test-select-probability-mass
  (is (= {:d 0.4, :c 0.3}
         (select-probability-mass 0.7
                                  {:a 0.1
                                   :b 0.2
                                   :c 0.3
                                   :d 0.4})))

  (is (= {:d 0.4, :c 0.3, :b 0.2}
         (select-probability-mass 0.9
                                  {:a 0.1
                                   :b 0.2
                                   :c 0.3
                                   :d 0.4}))))

(defn filter-target-text [text characters]
  (apply str (filter (conj (set characters) " ")
                     (map str (string/lower-case text)))))

(defn filter-target-text-without-space [text characters]
  (apply str
         (filter (set characters)
                 (map str (string/lower-case text)))))

(defn normalized-n-gram-distribution [n text characters]
  (->> (filter-target-text text characters)
       (n-gram-distribution n)
       normalize-distribution))

(defn normalized-digram-distribution
  ([text]
   (normalized-digram-distribution text finnish-characters))
  ([text characters]
   (normalized-n-gram-distribution 2 text characters)))

(defn normalized-trigram-distribution [text characters]
  (normalized-n-gram-distribution 3 text characters))

(defn normalized-character-distribution [text characters]
  (->> (filter-target-text-without-space text characters)
       character-distribution
       normalize-distribution))

(defn text-statistics
  ([text]
   (text-statistics text finnish-characters))

  ([text characters]
   {:digram-distribution (select-probability-mass 0.95
                                                  (normalized-n-gram-distribution 2 text characters))

    :trigram-distribution (select-probability-mass 0.50
                                                   (normalized-n-gram-distribution 3 text characters))

    :character-distribution (normalized-character-distribution text characters)}))

(defonce english-statistics (assoc (text-statistics (slurp "temp/text/the-hacker-crackdown.txt")
                                                    english-characters)
                                   :name "en"))

(defonce english-statistics-with-diacritics (assoc (text-statistics (str "åäö" (slurp "temp/text/the-hacker-crackdown.txt"))
                                                                    (concat ["ö" "ä" "å"] english-characters))
                                                   :name "end"))

(defonce finnish-statistics (assoc (text-statistics (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                    finnish-characters)
                                   :name "fi"))

(defonce hybrid-statistics (assoc (text-statistics (str (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                        (slurp "temp/text/the-hacker-crackdown.txt"))
                                                   finnish-characters)
                                  :name "hy"))

(comment
  (defonce finnish-statistics (read-string (slurp (io/resource "finnish-statistics.edn"))))

  (defonce hybrid-statistics (read-string (slurp (io/resource "hybrid-statistics.edn"))))

  (spit "resources/finnish-statistics.edn"
        (pr-str (assoc (text-statistics (str (string/join " " excercise/english-words))
                                        finnish-characters)
                       :name "fi")))

  (spit "resources/finnish-statistics.edn"
        (pr-str (assoc (text-statistics (str (string/join " " excercise/finnish-words))
                                        finnish-characters)
                       :name "fi")))

  (spit "resources/hybrid-statistics.edn"
        (pr-str (assoc (text-statistics (str (string/join " " excercise/english-words)
                                             (string/join " " excercise/finnish-words))
                                        finnish-characters)
                       :name "hy")))
  )


;; RATING

(def finger-hand {0 0
                  1 0
                  2 0
                  3 0
                  4 1
                  5 1
                  6 1
                  7 1})

(def finger-type
  {0 :pinky
   1 :ring
   2 :middle
   3 :index
   4 :index
   5 :middle
   6 :ring
   7 :pinky})

(defn rate-finger-type [key]
  (assoc (case (finger-type (:finger key))
           :index {:effort 0}
           :middle {:effort 0.25}
           :ring {:effort 0.5}
           :pinky {:effort 1})
         :label (finger-type (:finger key))
         :rating :finger-type))

(deftest test-rate-finger-type
  (is (= {:effort 0.25, :label :middle, :rating :finger-type}
         (rate-finger-type {:finger 2}))))

(defn key-rating [key]
  {:rating :key
   :label (:class key)
   :effort (key-class-effort (:class key))})

(def ^:dynamic multipliers {:digram-roll 1
                            :trigram-roll 1
                            :key-rating 1
                            :finger-type 1
                            :horizontal-movement 1
                            :vertical-movement 1
                            :hand-balance 1})

(defn multiplier [multiplier-key]
  (or (get multipliers multiplier-key)
      1))

(defn multiply-effort [multiplier-key rating]
  (update rating
          :effort
          (partial * (multiplier multiplier-key))))

(defn rate-key [key]
  [(multiply-effort :finger-type (rate-finger-type key))
   (multiply-effort :key-rating (key-rating key))])

(defn hands [key-sequence]
  (set (map (comp finger-hand :finger)
            key-sequence)))

(defn same-hand? [key-pair]
  (= 1 (count (hands key-pair))))

(defn same-finger? [key-pair]
  (= 1 (count (set (map :finger key-pair)))))

(defn different-fingers? [key-sequence]
  (= (count key-sequence)
     (count (set (map :finger key-sequence)))))

(defn same-column? [key-pair]
  (= 1 (count (set (map :column key-pair)))))

(defn left-hand? [key-sequence]
  (= #{0} (hands key-sequence)))

(defn right-hand? [key-sequence]
  (= #{1} (hands key-sequence)))

(defn same-row? [key-sequence]
  (= 1 (count (set (map :row key-sequence)))))

(defn two-row-leap? [key-pair]
  (= #{0 2} (set (map :row key-pair))))

(defn one-row-leap? [key-pair]
  (or (= #{0 1} (set (map :row key-pair)))
      (= #{1 2} (set (map :row key-pair)))))

(defn on-home-row? [key-sequence]
  (= #{1} (set (map :row key-sequence))))

(defn rate-vertical-movement [key-pair]
  (assoc (cond (not (same-hand? key-pair))
               {:label :different-hand
                :effort 0}

               (and (not (same-finger? key-pair))
                    (one-row-leap? key-pair))
               {:label :different-finger-one-row-leap
                :effort 0.1}

               (and (not (same-finger? key-pair))
                    (two-row-leap? key-pair))
               {:label :different-finger-two-row-leap
                :effort 0.2}

               (and (same-finger? key-pair)
                    (one-row-leap? key-pair))
               {:label :same-finger-one-row-leap
                :effort 0.75}

               (and (same-finger? key-pair)
                    (two-row-leap? key-pair))
               {:label :same-finger-two-row-leap
                :effort 1}

               :else
               {:label :same-row
                :effort 0})
         :rating :vertical-movement))

(deftest test-rate-vertical-movement
  (is (= {:label :different-hand, :effort 0, :rating :vertical-movement}
         (rate-vertical-movement [(qwerty-character-to-key "f")
                                  (qwerty-character-to-key "j")])))

  (is (= {:label :different-finger-one-row-leap, :effort 0.25, :rating :vertical-movement}
         (rate-vertical-movement [(qwerty-character-to-key "j")
                                  (qwerty-character-to-key "i")])))

  (is (= {:label :different-finger-two-row-leap, :effort 0.5, :rating :vertical-movement}
         (rate-vertical-movement [(qwerty-character-to-key "i")
                                  (qwerty-character-to-key "m")])))

  (is (= {:label :same-finger-one-row-leap, :effort 0.75, :rating :vertical-movement}
         (rate-vertical-movement [(qwerty-character-to-key "j")
                                  (qwerty-character-to-key "u")])))

  (is (= {:label :same-finger-two-row-leap, :effort 1, :rating :vertical-movement}
         (rate-vertical-movement [(qwerty-character-to-key "m")
                                  (qwerty-character-to-key "u")]))))

(defn rate-horizontal-movement [key-pair]
  (assoc (cond (not (same-finger? key-pair))
               {:label :different-finger
                :effort 0}

               (same-column? key-pair)
               {:label :same-column
                :effort 0}

               :else
               {:label :same-finger-different-column
                :effort 1})
         :rating :horizontal-movement))


(defn roll? [key-pair]
  (and (same-hand? key-pair)
       (different-fingers? key-pair)))

(defn ascending? [sequence]
  (= sequence (sort sequence)))

(defn descending? [sequence]
  (= sequence
     (reverse (sort sequence))))

(defn invards-progression? [key-sequence]
  (or (and (left-hand? key-sequence)
           (ascending? (map :finger key-sequence)))
      (and (right-hand? key-sequence)
           (descending? (map :finger key-sequence)))))

(defn invards-roll? [key-sequence]
  (and (roll? key-sequence)
       (invards-progression? key-sequence)))

;; (defn adjacent-fingers? [key-pair]
;;   (= 1 (abs (- (:finger (first key-pair))
;;                (:finger (second key-pair))))))

(defn adjacent-number-pair? [[number-1 number-2]]
  (or (= number-1 number-2)
      (= 1 (abs (- number-1 number-2)))))

(deftest test-adjacent-number-pair?
  (is (adjacent-number-pair? [1 2]))
  (is (adjacent-number-pair? [1 1]))
  (is (not (adjacent-number-pair? [1 3]))))

(defn adjacent-numbers? [numbers]
  (every? adjacent-number-pair?
          (partition 2 1 numbers)))

(deftest test-adjacent-numbers?
  (is (adjacent-numbers? [0
                          1]))

  (is (adjacent-numbers? [0
                          1
                          2]))

  (is (adjacent-numbers? [1
                          0]))

  (is (not (adjacent-numbers? [0
                               2])))

  (is (not (adjacent-numbers? [0
                               1
                               3])))

  (is (not (adjacent-numbers? [2
                               0]))))

(defn adjacent-fingers? [key-sequence]
  (adjacent-numbers? (map :finger key-sequence)))

(defn two-adjacent-rows? [key-sequence]

  (let [rows (map :row key-sequence)]
    (and (adjacent-numbers? rows)
         (= 2 (count (set rows))))))

(deftest test-two-adjacent-rows?
  (is (two-adjacent-rows? [{:row 1}
                           {:row 1}
                           {:row 2}]))

  (is (not (two-adjacent-rows? [{:row 0}
                                {:row 1}
                                {:row 2}])))

  (is (not (two-adjacent-rows? [{:row 0}
                                {:row 2}]))))

;; (defn rate-roll [key-pair]
;;   (assoc (cond (and (invards-roll? key-pair)
;;                     (on-home-row? key-pair)
;;                     (adjacent-fingers? key-pair))
;;                {:label :adjacent-invards-roll-on-home-row
;;                 :effort 1}

;;                (and (invards-roll? key-pair)
;;                     (on-home-row? key-pair))
;;                {:label :invards-roll-on-home-row
;;                 :effort 1.15}

;;                (and (roll? key-pair)
;;                     (on-home-row? key-pair)
;;                     (adjacent-fingers? key-pair))
;;                {:label :adjacent-outwards-roll-on-home-row
;;                 :effort 1.20}

;;                (and (roll? key-pair)
;;                     (on-home-row? key-pair))
;;                {:label :outwards-roll-on-home-row
;;                 :effort 1.25}

;;                (invards-roll? key-pair)
;;                {:label :inwards-roll
;;                 :effort 1.5}

;;                (roll? key-pair)
;;                {:label :outwards-roll
;;                 :effort 1.75}

;;                :else
;;                {:label :no-roll
;;                 :effort 2})
;;          :rating :roll))

;; (deftest test-rate-roll
;;   (is (= {:label :adjacent-invards-roll-on-home-row, :effort 1, :rating :roll}
;;          (rate-roll [(qwerty-character-to-key "d")
;;                      (qwerty-character-to-key "f")])))

;;   (is (= {:label :adjacent-outwards-roll-on-home-row, :effort 1.20, :rating :roll}
;;          (rate-roll [(qwerty-character-to-key "f")
;;                      (qwerty-character-to-key "d")])))

;;   (is (= {:label :inwards-roll, :effort 1.5, :rating :roll}
;;          (rate-roll [(qwerty-character-to-key "x")
;;                      (qwerty-character-to-key "c")])))

;;   (is (= {:label :outwards-roll, :effort 1.75, :rating :roll}
;;          (rate-roll [(qwerty-character-to-key "c")
;;                      (qwerty-character-to-key "x")])))

;;   (is (= {:label :no-roll, :effort 2, :rating :roll}
;;          (rate-roll [(qwerty-character-to-key "f")
;;                      (qwerty-character-to-key "j")]))))

(defn rate-n-gram-roll [key-sequence]
  (let [rating (keyword (str (count key-sequence) "-roll"))]
    (if (not (and (same-hand? key-sequence)
                  (different-fingers? key-sequence)
                  (or (ascending? (map :finger key-sequence))
                      (descending? (map :finger key-sequence)))
                  (or (same-row? key-sequence)
                      (two-adjacent-rows? key-sequence))))
      {:rating rating
       :effort 1
       :label :no-roll}
      (let [effort-reductions (-> []
                                  (cond->
                                      (adjacent-fingers?  key-sequence)
                                    (conj {:effort-reduction-label :adjacent
                                           :effort-reduction 2/10})

                                    (not (adjacent-fingers?  key-sequence))
                                    (conj {:effort-reduction-label :unadjacent
                                           :effort-reduction 0})

                                    (invards-progression? key-sequence)
                                    (conj {:effort-reduction-label :invards
                                           :effort-reduction 1/10})

                                    (not (invards-progression? key-sequence))
                                    (conj {:effort-reduction-label :outwards
                                           :effort-reduction 0})

                                    (on-home-row? key-sequence)
                                    (conj {:effort-reduction-label :home-row
                                           :effort-reduction 2/10})

                                    (and (not (on-home-row? key-sequence))
                                         (same-row? key-sequence))
                                    (conj {:effort-reduction-label :same-non-home-row
                                           :effort-reduction 1/20})

                                    (and (not (on-home-row? key-sequence))
                                         (not (same-row? key-sequence))
                                         (two-adjacent-rows? key-sequence))
                                    (conj {:effort-reduction-label :two-adjacent-rows
                                           :effort-reduction 0})))]
        {:rating rating
         :effort (double (- 1/2 (reduce + (remove nil? (map :effort-reduction effort-reductions)))))
         :label (keyword (str (string/join "-" (map (comp name :effort-reduction-label)
                                                    effort-reductions))
                              "-roll"))}))))

(deftest test-rate-n-gram-roll
  (is (= {:rating :3-roll, :effort 0.0, :label :adjacent-invards-home-row-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "a")
                            (qwerty-character-to-key "s")
                            (qwerty-character-to-key "d")])))

  (is (= {:rating :3-roll, :effort 0.1, :label :adjacent-outwards-home-row-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "f")
                            (qwerty-character-to-key "d")
                            (qwerty-character-to-key "s")])))

  (is (= {:rating :3-roll, :effort 0.15, :label :adjacent-invards-same-non-home-row-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "z")
                            (qwerty-character-to-key "x")
                            (qwerty-character-to-key "c")])))

  (is (= {:rating :3-roll, :effort 0.2, :label :unadjacent-invards-home-row-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "a")
                            (qwerty-character-to-key "d")
                            (qwerty-character-to-key "f")])))

  (is (= {:rating :3-roll, :effort 0.2, :label :adjacent-invards-two-adjacent-rows-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "a")
                            (qwerty-character-to-key "e")
                            (qwerty-character-to-key "r")])))

  (is (= {:rating :3-roll, :effort 0.3, :label :adjacent-outwards-two-adjacent-rows-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "r")
                            (qwerty-character-to-key "e")
                            (qwerty-character-to-key "a")])))

  (is (= {:rating :3-roll, :effort 0.3, :label :unadjacent-outwards-home-row-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "f")
                            (qwerty-character-to-key "d")
                            (qwerty-character-to-key "a")])))

  (is (= {:rating :3-roll, :effort 0.45, :label :unadjacent-outwards-same-non-home-row-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "c")
                            (qwerty-character-to-key "x")
                            (qwerty-character-to-key "<")])))

  (is (= {:rating :3-roll, :effort 0.35, :label :unadjacent-invards-same-non-home-row-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "<")
                            (qwerty-character-to-key "x")
                            (qwerty-character-to-key "c")])))



  (is (= {:rating :2-roll, :effort 1, :label :no-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "f")
                            (qwerty-character-to-key "j")])))

  (is (= {:rating :3-roll, :effort 1, :label :no-roll}
         (rate-n-gram-roll [(qwerty-character-to-key "a")
                            (qwerty-character-to-key "e")
                            (qwerty-character-to-key "c")]))))

(defn rate-key-pair [key-pair]
  [(multiply-effort :vertical-movement (rate-vertical-movement key-pair))
   (multiply-effort :horizontal-movement (rate-horizontal-movement key-pair))
   (multiply-effort :digram-roll (rate-n-gram-roll key-pair))])

(defn rate-key-triple [key-triple]
  [(multiply-effort :trigram-roll (rate-n-gram-roll key-triple))])

(defn rate-distribution [rate distribution]
  (reduce +
          (for [[value propability] distribution]
            (* propability
               (rate value)))))

(defn total-effort [ratings]
  (apply + (map :effort ratings)))

(defn rate-hand-balance [character-distribution character-to-key]
  (- 1
     (->> (for [[character propability] character-distribution]
            [(-> character
                 (character-to-key)
                 (:finger)
                 (finger-hand))
             propability])
          (map #(apply hash-map %))
          (apply merge-with +)
          (vals)
          (sort)
          (apply /))))

(deftest test-rate-hand-balance
  (is (= 1.0
         (rate-hand-balance {"a" 0.5
                             "b" 0.5}
                            {"a" {:finger 3}
                             "b" {:finger 4}})))

  (is (= 0.8181818181818181
         (rate-hand-balance {"a" 0.45
                             "b" 0.55}
                            {"a" {:finger 3}
                             "b" {:finger 4}})))

  (is (= 0.6666666666666667
         (rate-hand-balance {"a" 0.4
                             "b" 0.6}
                            {"a" {:finger 3}
                             "b" {:finger 4}}))))

(defn rate-layout [text-statistics layout]
  (assert (and (set? layout)
               (every? :cocoa-key-code layout)))

  (let [character-to-key (comp cocoa-key-code-to-key
                               (layout-to-character-to-cocoa-key-code layout))]
    (+ (rate-distribution (fn [digram]
                            (total-effort (rate-key-pair (map character-to-key digram))))
                          (:digram-distribution text-statistics))
       #_(rate-distribution (fn [trigram]
                              (total-effort (rate-key-triple (map character-to-key trigram))))
                            (:trigram-distribution text-statistics))
       (rate-distribution (fn [character]
                            (total-effort (rate-key (character-to-key character))))
                          (:character-distribution text-statistics))
       (* (multiplier :hand-balance)
          (rate-hand-balance (:character-distribution text-statistics)
                             character-to-key)))))

(deftest test-rate-layout
  (is (= 2.5708333333333333
         (rate-layout (text-statistics "hello" ["h" "e" "l" "o"])
                      qwerty)))

  (is (= 1.1883333333333335
         (rate-layout (text-statistics "hello" ["h" "e" "l" "o"])
                      colemak-dh)))

  (is (= 1.3166666666666667
         (rate-layout (text-statistics "hello" ["h" "e" "l" "o"])
                      (layout-from-qwerty {"h" "k"
                                           "e" "j"
                                           "l" "d"
                                           "o" "f"})))))

(defn describe-layout-rating [text-statistics layout]
  (let [character-to-key (comp cocoa-key-code-to-key
                               (layout-to-character-to-cocoa-key-code layout))]
    {:distributions {:digrams (doall (for [digram-propability (->> (:digram-distribution text-statistics)
                                                                   (sort-by second)
                                                                   (reverse))]
                                       {:propability digram-propability
                                        :ratings (rate-key-pair (map character-to-key
                                                                     (first digram-propability)))}))
                     :trigrams (doall (for [trigram-propability (->> (:trigram-distribution text-statistics)
                                                                     (sort-by second)
                                                                     (reverse))]
                                        {:propability trigram-propability
                                         :ratings (rate-key-triple (map character-to-key
                                                                        (first trigram-propability)))}))
                     :characters (doall (for [character-propability (->> (:character-distribution text-statistics)
                                                                         (sort-by second)
                                                                         (reverse))]
                                          {:propability character-propability
                                           :ratings (rate-key (character-to-key (first character-propability)))}))}
     :holistic-ratings {:hand-balance (* (multiplier :hand-balance)
                                         (rate-hand-balance (:character-distribution text-statistics)
                                                            character-to-key))}}))

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
           :holistic-ratings {:hand-balance 0.25}}
         (describe-layout-rating (text-statistics "hello")
                                 qwerty)))

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
           :holistic-ratings {:hand-balance 0.6666666666666666}}
         (describe-layout-rating (text-statistics "hello")
                                 (layout-from-qwerty {"h" "k"
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
  (let [summary (meldey/map-vals summarize-rating-aspect
                                 (:distributions rating-description))]
    (assoc summary
           :total (->> (conj (vals summary)
                             (:holistic-ratings rating-description))
                       (map (fn [ratings]
                              (reduce + (vals ratings))))
                       (reduce +))
           :holistic-ratings (:holistic-ratings rating-description))))

(deftest test-summarize-rating-description
  (is (= {:digrams {:vertical-movement 0.0, :horizontal-movement 0.0, :2-roll 0.5},
          :trigrams {:3-roll 0.3333333333333333},
          :characters {:finger-type 0.15000000000000002, :key 0.0},
          :total 1.65,
          :holistic-ratings {:hand-balance 0.6666666666666666}}
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
                                         :holistic-ratings {:hand-balance 0.6666666666666666}}))))

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

(defn random-layout
  ([]
   (random-layout finnish-characters))
  ([characters]
   (loop [remaining-cocoa-key-codes (sort (map :cocoa-key-code (remove :disabled? keyboard-keys)))
          remaining-characters characters
          layout #{}]
     (if (empty? remaining-characters)
       (set/union layout
                  (into #{}
                        (for [cocoa-key-code remaining-cocoa-key-codes]
                          {:character ""
                           :cocoa-key-code cocoa-key-code})))
       (let [character (first remaining-characters)
             cocoa-key-code (rand-nth remaining-cocoa-key-codes)]
         (recur (remove #{cocoa-key-code} remaining-cocoa-key-codes)
                (rest remaining-characters)
                (conj layout {:character character
                              :cocoa-key-code cocoa-key-code})))))))

(comment
  (set/difference (set (map :character (random-layout (keys (:character-distribution hybrid-statistics)))))
                  (set (keys (:character-distribution hybrid-statistics))))
  )

(defn create-random-double-function
  ([] (create-random-double-function nil))
  ([seed] (let [random (if (some? seed)
                         (Random. seed)
                         (Random.))]
            (fn []
              (.nextDouble random)))))

(def ^:dynamic random-double (create-random-double-function))

(defmacro with-fixed-random-seed [& body]
  `(binding [random-double (create-random-double-function 1)]
     ~@body))

(defn pick-random [collection]
  (nth collection
       (* (random-double)
          (count collection))))

(defn crossbreed-layouts [layout-1 layout-2]
  (loop [cocoa-key-code-to-character-1 (layout-to-cocoa-key-code-to-character layout-1)
         cocoa-key-code-to-character-2 (layout-to-cocoa-key-code-to-character layout-2)
         new-layout #{}
         cocoa-key-codes (map :cocoa-key-code layout-1)]
    (if (empty? cocoa-key-codes)
      new-layout
      (let [cocoa-key-code (first cocoa-key-codes)
            character (if (< 0.5 (random-double))
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
         (with-fixed-random-seed
           (crossbreed-layouts #{{:character "a" :cocoa-key-code 0}
                                 {:character "b" :cocoa-key-code 1}}
                               #{{:character "a" :cocoa-key-code 1}
                                 {:character "b" :cocoa-key-code 0}}))))

  (is (= #{{:cocoa-key-code 3, :character "c"}
           {:cocoa-key-code 0, :character "a"}
           {:cocoa-key-code 1, :character "b"}}
         (with-fixed-random-seed
           (crossbreed-layouts #{{:character "a" :cocoa-key-code 0}
                                 {:character "b" :cocoa-key-code 1}
                                 {:character "c" :cocoa-key-code 3}}
                               #{{:character "a" :cocoa-key-code 3}
                                 {:character "b" :cocoa-key-code 0}
                                 {:character "c" :cocoa-key-code 1}})))))

(defn swap-mappings [layout mapping-1 mapping-2]
  (-> layout
      (disj mapping-1
            mapping-2)
      (conj (assoc mapping-1 :cocoa-key-code (:cocoa-key-code mapping-2)))
      (conj (assoc mapping-2 :cocoa-key-code (:cocoa-key-code mapping-1)))))

(defn gradient-descent-one [text-statistics layout]
  (assert (not (nil? layout)))
  (let [current-effort (rate-layout text-statistics layout)
        {:keys [effort layout]} (->> (for [mapping-1 layout
                                           mapping-2 layout]
                                       (-> layout
                                           (swap-mappings mapping-1
                                                          mapping-2)))
                                     (pmap (fn [layout]
                                             {:effort (rate-layout text-statistics
                                                                   layout)
                                              :layout layout}))
                                     (sort-by :effort)
                                     (first))]
    (when (> current-effort effort)
      layout)))

(defn gradient-descent-all [text-statistics layout]
  (loop [current-layout layout
         next-layout (gradient-descent-one text-statistics
                                           layout)]

    (if (nil? next-layout)
      (set current-layout)
      (recur next-layout
             (gradient-descent-one text-statistics
                                   next-layout)))))

(deftest test-gradient-descent-all
  (let [qwerty-key-code (fn [character]
                          ((layout-to-character-to-cocoa-key-code qwerty) character))
        qwerty-character (fn [cocoa-key-code]
                           ((layout-to-cocoa-key-code-to-character qwerty) cocoa-key-code))
        make-readable (fn [layout]
                        (sort-by :character
                                 (map (fn [mapping]
                                        {:character (:character mapping)
                                         :qwerty-character (qwerty-character (:cocoa-key-code mapping))})
                                      layout)))]

    (is (= '({:character "a", :qwerty-character "f"}
             {:character "b", :qwerty-character "q"}
             {:character "c", :qwerty-character "e"}
             {:character "x", :qwerty-character "w"}
             {:character "y", :qwerty-character "j"}
             {:character "z", :qwerty-character "k"})
           (make-readable (gradient-descent-all (text-statistics "abc" ["a" "b" "c"])
                                                #{{:character "a" :cocoa-key-code (qwerty-key-code "q")}
                                                  {:character "b" :cocoa-key-code (qwerty-key-code "w")}
                                                  {:character "c" :cocoa-key-code (qwerty-key-code "e")}
                                                  {:character "x" :cocoa-key-code (qwerty-key-code "f")}
                                                  {:character "y" :cocoa-key-code (qwerty-key-code "j")}
                                                  {:character "z" :cocoa-key-code (qwerty-key-code "k")}}))))

    (is (= '({:character "a", :qwerty-character "q"}
             {:character "b", :qwerty-character "f"}
             {:character "c", :qwerty-character "e"}
             {:character "x", :qwerty-character "w"}
             {:character "y", :qwerty-character "j"}
             {:character "z", :qwerty-character "k"})
           (make-readable
            (gradient-descent-one (text-statistics "abc" ["a" "b" "c"])
                                  #{{:character "a" :cocoa-key-code (qwerty-key-code "q")}
                                    {:character "b" :cocoa-key-code (qwerty-key-code "w")}
                                    {:character "c" :cocoa-key-code (qwerty-key-code "e")}
                                    {:character "x" :cocoa-key-code (qwerty-key-code "f")}
                                    {:character "y" :cocoa-key-code (qwerty-key-code "j")}
                                    {:character "z" :cocoa-key-code (qwerty-key-code "k")}}))))))


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

(defn black-background [contents]
  (layouts/superimpose (visuals/rectangle-2 {:fill-color [0 0 0 255]})
                       contents))

(defonce event-channel-atom (atom nil))

(defn refresh-view! []
  (when @event-channel-atom
    (async/>!! @event-channel-atom
               {:type :redraw})))

(comment
  (reset! event-channel-atom nil)
  )

(defn start-view [view & {:keys [join?]}]
  (reset! event-channel-atom
          (application/start-application view
                                         :on-exit #(reset! event-channel-atom nil)
                                         :join? join?)))

(defn best-rating [layouts-with-ratings]
  (first (sort (map second layouts-with-ratings))))

(def ^:dynamic generation-size 50)
(def ^:dynamic number-of-genrations 1000)
(def ^:dynamic maximum-number-of-generations-without-improvement 1000)

(defonce running-atom? (atom true))

(def initial-optimization-state {})

(defonce optimization-state-atom (atom initial-optimization-state))

;; (defn weighted-random [distribution]
;;   (let [max-sum (* (reduce + (map second distribution))
;;                    (rand))]
;;     (loop [sum 0
;;            distribution distribution]
;;       (let [propability (first distribution)
;;             sum (+ sum (second propability))]
;;         (if (<= (abs sum)
;;                 (abs max-sum))
;;           (recur sum
;;                  (rest distribution))
;;           (first propability))))))

(defn weighted-random [distribution temperature]
  (let [temperature-adjusted-distribution (map (fn [[value propability]]
                                                 [value (Math/pow propability
                                                                  (/ 1.0 temperature))])
                                               distribution)
        total (reduce + (map second temperature-adjusted-distribution))
        propability-limit (* total
                             (random-double))]
    (loop [sum 0
           [[value propability] & rest] temperature-adjusted-distribution]
      (if (<= (+ sum propability)
              propability-limit)
        (recur (+ sum propability) rest)
        value))))

(deftest test-weighted-random
  (is (= :a
         (binding [random-double (create-random-double-function 1)]
           (weighted-random [[:a 0.7]
                             [:b 0.3]]
                            0.1))))

  (is (= :b
         (binding [random-double (create-random-double-function 1)]
           (weighted-random [[:a 0.7]
                             [:b 0.3]]
                            100)))))

(comment
  (frequencies (repeatedly 1000 #(weighted-random [[:a 0.7]
                                                   [:b 0.3]]
                                                  0.3)))
  )

(defn invert-distribution [distribution]
  (map (fn [[value propability]]
         [value (/ 1.0
                   (+ propability 1e-9))])
       distribution))

(deftest test-invert-distribution
  (is (= '([:a 0.19999999996]
           [:b 0.09999999999])
         (invert-distribution [[:a 5]
                               [:b 10]]))))

(defn normalize-distribution [distribution]
  (let [total (reduce + (map second distribution))]
    (map (fn [[value propability]]
           [value (/ propability
                     total)])
         distribution)))

(deftest test-normalize-distribution
  (is (= '([:a 1/3] [:b 2/3])
         (normalize-distribution [[:a 5]
                                  [:b 10]]))))

(defn ratings-to-distribution [ratings]
  (->> ratings
       (invert-distribution)
       (normalize-distribution)))

(deftest test-ratings-to-distribution
  (is (= '([:a 0.6666666666444444]
           [:b 0.33333333335555554])
         (ratings-to-distribution [[:a 5]
                                   [:b 10]]))))

(deftest test-ratings-to-distribution
  (is (= '([:b 0.5999999999200001]
           [:a 0.40000000008000003])
         (ratings-to-distribution #{[:a 1.5]
                                    [:b 1.0]}))))

(defn layouts-to-ratings [text-statistics layouts]
  (pmap (fn [layout]
          [layout (rate-layout text-statistics layout)])
        layouts))

(defn next-generation-ratings [current-generation-ratings
                               text-statistics
                               {:keys [population-size
                                       elite-proportion
                                       parent-selection-temperature
                                       mutation-propability
                                       random-layout-proportion]}]
  (let [elite-count (max 1 (int (Math/ceil (* population-size elite-proportion))))
        random-layout-count (max 1 (int (Math/ceil (* population-size random-layout-proportion))))
        child-count (- population-size
                       elite-count
                       random-layout-count)
        distribution (ratings-to-distribution current-generation-ratings)
        children (repeatedly child-count
                             (fn []
                               (let [child (crossbreed-layouts (weighted-random distribution parent-selection-temperature)
                                                               (weighted-random distribution parent-selection-temperature))
                                     child (if (< (rand)
                                                  mutation-propability)
                                             (mutate-layout child)
                                             child)]
                                 child)))]
    (concat (take elite-count
                  (sort-by second
                           current-generation-ratings))
            (->> (repeatedly random-layout-count
                             random-layout)
                 (layouts-to-ratings text-statistics))
            (layouts-to-ratings text-statistics children))))

(deftest test-next-generation-ratings
  (is (= 5 (count (with-fixed-random-seed
                    (next-generation-ratings (->> (repeatedly 2 random-layout)
                                                  (layouts-to-ratings hybrid-statistics))
                                             hybrid-statistics
                                             {:population-size 5
                                              :elite-proportion 0.2
                                              :parent-selection-temperature 1
                                              :mutation-propability 0.5
                                              :random-layout-proportion 0.1}))))))

(defn linear-mapping [base minimum maximum slope x]
  (min maximum
       (max minimum
            (+ base (* slope x)))))

(defn next-generation-parameters [generations-since-last-improvement & [{:keys [population-size
                                                                                elite-proportion-slope
                                                                                minimum-elite-proportion
                                                                                maximum-elite-proportion
                                                                                parent-selection-temperature-slope
                                                                                mutation-propability-slope
                                                                                minimum-mutation-propability
                                                                                random-layout-proportion-slope
                                                                                minimum-random-layout-proportion]
                                                                         :or {population-size 500
                                                                              elite-proportion-slope 0.01
                                                                              minimum-elite-proportion 0.05
                                                                              maximum-elite-proportion 0.15
                                                                              parent-selection-temperature-slope 0.1
                                                                              mutation-propability-slope 0.01
                                                                              minimum-mutation-propability 0.05
                                                                              random-layout-proportion-slope 0.01
                                                                              minimum-random-layout-proportion 0.05}}]]
  {:population-size population-size
   ;; hot-right-now TODO: remove me
   :elite-proportion (linear-mapping maximum-elite-proportion
                                     minimum-elite-proportion
                                     maximum-elite-proportion
                                     (- elite-proportion-slope)
                                     generations-since-last-improvement)

   :parent-selection-temperature (linear-mapping 1.0
                                                 1.0
                                                 10.0
                                                 parent-selection-temperature-slope
                                                 generations-since-last-improvement)
   :mutation-propability (linear-mapping minimum-mutation-propability
                                         minimum-mutation-propability
                                         0.2
                                         mutation-propability-slope
                                         generations-since-last-improvement)

   :random-layout-proportion (linear-mapping minimum-random-layout-proportion
                                             minimum-random-layout-proportion
                                             0.5
                                             random-layout-proportion-slope
                                             generations-since-last-improvement)})

(defonce optimization-history-atom (atom []))
(defonce stop-requested?-atom (atom false))

(defn optimize-layout [text-statistics]
  (reset! optimization-history-atom [])
  (reset! stop-requested?-atom false)

  (loop [state {:generation-number 0
                :last-improved-generation-number 0
                :ratings (->> (repeatedly 2 random-layout)
                              (layouts-to-ratings text-statistics))}]

    (let [history-item (-> state
                           (dissoc :ratings)
                           (assoc :best-rating (best-rating (:ratings state))))]
      (swap! optimization-history-atom conj
             history-item)

      (refresh-view!)

      (when (= 0 (mod (:generation-number state)
                      10))
        (prn (merge history-item
                    (next-generation-parameters (- (:generation-number state)
                                                   (:last-improved-generation-number state)))))))

    (let [next-generation-ratings (next-generation-ratings (:ratings state)
                                                           text-statistics
                                                           (next-generation-parameters (- (:generation-number state)
                                                                                          (:last-improved-generation-number state))))]

      (if @stop-requested?-atom
        (println "stopped")
        (recur {:generation-number (inc (:generation-number state))
                :last-improved-generation-number (if (< (best-rating next-generation-ratings)
                                                        (best-rating (:ratings state)))
                                                   (:generation-number state)
                                                   (:last-improved-generation-number state))
                :ratings next-generation-ratings})))))




(defn grid [size width height]
  (layouts/superimpose (for [i (range (/ height size))]
                         (path/path [100 100 100 255]
                                    3
                                    [{:x 0
                                      :y (* size i)}
                                     {:x width
                                      :y (* size i)}]))))



(defn graph [data]
  (layouts/superimpose (grid 10 100 200)
                       (path/path [100 100 100 255]
                                  2
                                  data)))
(comment
  (merge-with *
              {:a 2}
              {:a 3})
  ) ;; TODO: remove me

(defn scale-point [scale point]
  (merge-with * scale point))

(defn add-points [point-1 point-2]
  (merge-with + point-1 point-2))

(defn scale-to-view [width height points]
  (map (partial scale-point {:x (double (/ width
                                           (max 1e-9
                                                (- (apply max (map :x points))
                                                   (apply min (map :x points))))))
                             :y (double (/ height
                                           (max 1e-9
                                                (- (apply max (map :y points))
                                                   (apply min (map :y points))))))})
       points))

(deftest test-scale-to-view
  (is (= '({:x 0, :y 0}
           {:x 10, :y 10})
         (scale-to-view 10 10 [{:x 0
                                :y 0}
                               {:x 1
                                :y 1}])))

  (is (= '({:x 10.0, :y 2.5}
           {:x 20.0, :y 12.5})
         (scale-to-view 10 10 [{:x 1
                                :y 1}
                               {:x 2
                                :y 5}]))))

(defn move-to-origin [points]
  (map (partial add-points
                {:x (- (apply min (map :x points)))
                 :y (- (apply min (map :y points)))})
       points))

(deftest test-move-to-origin
  (is (= '({:x 0.0, :y 0.0}
           {:x 10.0, :y 10.0})
         (move-to-origin '({:x 10.0, :y 2.5}
                           {:x 20.0, :y 12.5})))))

(defn transform-left [points]
  (if (empty? points)
    points
    (map (partial add-points {:x (- (apply min (map :x points)))
                              :y 0})
         points)))

(defn property-view [key]
  (text (str key " " (key @optimization-state-atom))))

(defn optimization-progress-view []
  (black-background
   (layouts/with-margin 10
     (if (empty? @optimization-history-atom)
       (black-background (text "no history"))
       (let [latest-history-item (last @optimization-history-atom)
             enrich-history-item (fn [history-item]
                                   (-> history-item
                                       (merge (next-generation-parameters (- (:generation-number history-item)
                                                                             (:last-improved-generation-number history-item))))
                                       (assoc :generations-since-last-improvement
                                              (- (:generation-number history-item)
                                                 (:last-improved-generation-number history-item)))))
             enriched-history-items (->> @optimization-history-atom
                                         (map enrich-history-item))

             displayed-keys [:generation-number
                             :best-rating
                             :elite-proportion
                             :parent-selection-temperature

                             :mutation-propability
                             :random-layout-proportion
                             :generations-since-last-improvement]
             index-to-color (fn [index]
                              (concat (color/hsl-to-rgb (* 360 (/ index
                                                                  (count displayed-keys)))
                                                        0.5
                                                        0.5)
                                      [1.0]))
             graph-height 1000]
         (apply layouts/vertically-2
                {:margin 10}
                (apply layouts/superimpose
                       (for [[index key] (map vector
                                              (range)
                                              displayed-keys)]
                         (layouts/with-margin 50
                           (path/path (index-to-color index)
                                      5
                                      (->> enriched-history-items
                                           (map (fn [history-item]
                                                  {:x (:generation-number history-item)
                                                   :y (key history-item)}))
                                           (scale-to-view graph-height graph-height)
                                           (map (partial scale-point {:x 1 :y -1}))
                                           (move-to-origin))))))

                (for [[index key] (map vector
                                       (range)
                                       displayed-keys)]
                  (text (str key " " (key (enrich-history-item latest-history-item))
                             " min: " (apply min (map key enriched-history-items))
                             " max: " (apply max (map key enriched-history-items)))
                        {:color (index-to-color index)}))))))))

;; hot-right-now TODO: remove me
(comment
  (start-view #'optimization-progress-view)

  (reset! optimization-history-atom [])
  @optimization-history-atom

  (.start (Thread. (fn [] (optimize-layout hybrid-statistics))))
  (reset! stop-requested?-atom true)

  (def saved-optimization-state @optimization-state-atom)


  @optimization-state-atom






  (prn (rate-layout hybrid-statistics
                    (optimize-layout hybrid-statistics)))

  ;; 1.0585756223599754 breeding with 0-2 mutations
  ;; 1.0586912307882292 only mutations, no breeding

  (rate-layout hybrid-statistics
               (:layout @latest-optimized-layout-atom))
  ;; => 1.038050953690223

  (rate-layout hybrid-statistics
               (gradient-descent-all hybrid-statistics
                                     (:layout @latest-optimized-layout-atom)))
  ;; => 1.038050953690223


  (let [layout #_(optimize-layout text-statistics)
        (gradient-descent-all text-statistics
                              #_(random-layout (keys (:character-distribution text-statistics)))
                              (optimize-layout text-statistics))]
    (println (rate-layout text-statistics
                          layout))
    layout)


  (repeatedly 2
              (fn []
                (let [text-statistics hybrid-statistics
                      ga-optimized-layout (optimize-layout text-statistics)
                      gd-optimized-layout (gradient-descent-all text-statistics
                                                                ga-optimized-layout)]
                  [(rate-layout text-statistics
                                ga-optimized-layout)
                   (rate-layout text-statistics
                                gd-optimized-layout)
                   gd-optimized-layout])))

  ;; => [1.5025953891375772 1.4977164329644994]

  1.5235971920136926
  1.5214783475220246

  (rate-layout hybrid-statistics
               ga-and-gd-optimized-hybrid-layout)
  ;; => 1.5022908057715776

  (let [text-statistics hybrid-statistics
        gd-optimized-layout (gradient-descent-all text-statistics
                                                  (random-layout finnish-characters))]
    [(rate-layout text-statistics
                  gd-optimized-layout)
     gd-optimized-layout])


  (format "%.2f" (double 0.123455))


  (select-probability-mass 0.95
                           (normalized-trigram-distribution (slurp "temp/text/the-hacker-crackdown.txt")))
  (count (reverse (sort-by second (select-probability-mass 0.7 (normalized-trigram-distribution (slurp "temp/text/the-hacker-crackdown.txt"))))))
  ;; => 735
  ;; => 2764
  ;; => 7470
  ;; => 6133
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

(defn optimize-layout-with-multipliers [multipliers text-statistics & [{:keys [initial-layout]}]]
  (binding [multipliers multipliers]
    (let [layout (optimize-layout text-statistics initial-layout)
          #_(gradient-descent-all text-statistics
                                  #_(random-layout (keys (:character-distribution text-statistics)))
                                  (optimize-layout text-statistics initial-layout))]
      ;; (println (rate-layout text-statistics
      ;;                       layout))
      layout)))


(defn optimize-named-layout-with-multipliers [multipliers statistics & [{:keys [initial-layout]}]]
  {:multipliers multipliers
   :statistics-name (:name statistics)
   :layout (optimize-layout-with-multipliers multipliers
                                             statistics
                                             {:initial-layout initial-layout})})

(comment
  (def optimized-test-layout (optimize-layout-with-multipliers multipliers hybrid-statistics))
  (start-view (fn []
                (black-background [keyboard-view
                                   (layout-to-cocoa-key-code-to-character optimized-test-layout)
                                   key-colors-for-fingers])))

  (set/difference (set (map :qwerty-character keyboard-keys))
                  (set (map :character optimized-test-layout)))

  )


(defn format-in-us-locale [format & arguments]
  (String/format Locale/US
                 format
                 (to-array arguments)))

(defn multipliers-to-layout-name [multipliers]
  (->> multipliers
       (medley/map-keys (fn [key]
                          (subs (name key)
                                0 2)))
       (medley/map-vals (fn [value]
                          (format-in-us-locale "%.1f" (float value))))
       (apply concat)
       (map str)
       (partition 2)
       (map (fn [[key val]]
              (str key ":" val)))
       (string/join " ")))

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
  (optimize-layout (filter-target-text target-text)
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


(def one-hand-finger-colors [[105 49 70 255]
                             [70 49 105 255]
                             [105 105 49 255]
                             [70 105 49 255]])

(def both-hands-finger-colors
  (into [] (concat one-hand-finger-colors
                   (reverse one-hand-finger-colors))))

(def key-colors-for-fingers (into {}
                                  (for [keyboard-key keyboard-keys]
                                    [(:cocoa-key-code keyboard-key)
                                     (get both-hands-finger-colors
                                          (:finger keyboard-key))])))

(def key-size 30)

(defn row-view [cocoa-key-codes cocoa-key-code-to-character key-color on-event]
  (layouts/horizontally-2 {:margin 1}
                          (for [cocoa-key-code cocoa-key-codes]
                            (let [character (cocoa-key-code-to-character cocoa-key-code)]
                              {:node (box (layouts/with-minimum-size key-size key-size (text character))
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

(defn layout-comparison-text [text]
  (visuals/text text
                {:font-size 30}))



(defn layout-editor [_layout-atom _key-colors _statistics]
  (let [state-atom (dependable-atom/atom {})]
    (fn [layout-atom key-colors statistics]
      (let [cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character @layout-atom)
            character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code @layout-atom)
            state @state-atom
            current-effort (rate-layout statistics
                                        @layout-atom)
            on-key-event (fn [event]
                           ;;                           (prn '(:type event) event) ;; TODO: remove me

                           (case (:type event)
                             :mouse-entered-character
                             (swap! state-atom assoc :character-under-mouse (:character event))
                             :mouse-left-character
                             (swap! state-atom dissoc :character-under-mouse (:character event))
                             :mouse-pressed
                             (if-let [selected-character (:selected-character state)]
                               (do (swap! layout-atom
                                          swap-mappings
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
                                                   (rate-layout statistics
                                                                (swap-mappings @layout-atom
                                                                               selected-mapping
                                                                               mapping))]))
                 all-efforts (conj (vals cocoa-key-code-to-effort)
                                   current-effort)
                 maximum-effort (apply max all-efforts)
                 minimum-effort (apply min all-efforts)]
             (layouts/vertically-2 {}
                                   (keyboard-view
                                    cocoa-key-code-to-character
                                    (merge key-colors-for-fingers
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
                                   (layout-comparison-text (string/join " "
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
                                 [keyboard-view
                                  cocoa-key-code-to-character
                                  (merge key-colors-for-fingers
                                         key-colors)
                                  {:on-key-event on-key-event}]
                                 (layout-comparison-text (str "effort: " (format-in-us-locale "%.3f" current-effort)))))


         :can-gain-focus? true}))))

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


(defn key-heat-map-view [_cocoa-key-code-to-character _character-to-cocoa-key-code _character-distribution]
  (let [state-atom (dependable-atom/atom {})]
    (fn [cocoa-key-code-to-character character-to-cocoa-key-code character-distribution]
      (layouts/vertically-2 {:margin 10}
                            (keyboard-view
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
                                                     key-colors-for-fingers)
                               #_(merge-with (fn [finger-color character-propability]
                                               (mix-colors (/ character-propability
                                                              largest-character-propability)
                                                           (multiply-color 0.0 finger-color)
                                                           [200 100 100 255]))
                                             key-colors-for-fingers
                                             (medley/map-keys character-to-cocoa-key-code
                                                              character-distribution)))
                             {:on-key-event (fn [event]
                                              (case (:type event)
                                                :mouse-entered-character
                                                (swap! state-atom assoc :character-under-mouse (:character event))
                                                :mouse-left-character
                                                (swap! state-atom dissoc :character-under-mouse (:character event))
                                                nil))})
                            (layout-comparison-text (if-let [character-under-mouse (:character-under-mouse @state-atom)]
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
                                                          (multiply-color (- 1 (* 0.3 index))
                                                                          [140 140 255 255])])

                                                       n-gram)))
        rating (rate-n-gram-roll (map (comp cocoa-key-code-to-key
                                            character-to-cocoa-key-code)
                                      n-gram))
        key-colors-for-fingers (medley/map-vals (partial multiply-color
                                                         (max 0.4 (- 1 (:effort rating))))
                                                key-colors-for-fingers)]
    (layouts/vertically-2 {} (layout-comparison-text (str (:effort rating) " " (apply str n-gram)))
                          [keyboard-view
                           cocoa-key-code-to-character
                           (merge key-colors-for-fingers
                                  key-highlight-color)])))

(defn n-gram-comparison-view [named-layout _n-gram-distribution]
  (let [cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character (:layout named-layout))
        character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code (:layout named-layout))]
    (fn [named-layout n-gram-distribution]
      (layouts/vertically-2 {:margin 10}
                            (layout-comparison-text (pr-str (:multipliers named-layout)))
                            (layouts/flow (for [n-gram (map first (take 50 (reverse (sort-by second n-gram-distribution))))]
                                            (layouts/with-margin 20 (ngram-view cocoa-key-code-to-character
                                                                                character-to-cocoa-key-code
                                                                                n-gram))))))))

(comment
  (start-view (fn []
                [#'n-gram-comparison-view (first @optimized-layouts-atom)
                 (:digram-distribution hybrid-statistics)]))
  )


(defn homerow-string [cocoa-key-code-to-character]
  (apply str (map cocoa-key-code-to-character [0 1 2 3 5])))

(deftest test-homerow-string
  (is (= "asdfg"
         (homerow-string (layout-to-cocoa-key-code-to-character qwerty)))))

(defn layout-name [layout]
  (str (if-let [name  (:name layout)]
         name
         (multipliers-to-layout-name (:multipliers layout)))
       "-"
       (homerow-string (layout-to-cocoa-key-code-to-character (:layout layout)))
       "-"
       (:statistics-name layout)))

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
                                                                                (:layout layout))]
                          (-> layout
                              (assoc :layout-rating-description layout-rating-description
                                     :summary (merge-summary (summarize-rating-description layout-rating-description))))))
              columns (for [column (into #{} (apply concat (map keys (map :summary layouts))))]
                        {:key column
                         :minimum (apply min (map column (map :summary layouts)))
                         :maximum (apply max (map column (map :summary layouts)))})]
          [layouts/grid (doall (concat [(concat [(layout-comparison-text "layout")]
                                                (for [column columns]
                                                  (on-click (fn []
                                                              (if (= column (:sort-column @state-atom))
                                                                (swap! state-atom update :sort-descending? not)
                                                                (swap! state-atom assoc :sort-column column)))
                                                            (cell (layout-comparison-text (name (:key column)))))))]
                                       (for [layout (-> (if (:sort-column @state-atom)
                                                          (sort-by (fn [layout]
                                                                     (get (:summary layout)
                                                                          (:key (or (:sort-column @state-atom)
                                                                                    (first columns)))))
                                                                   layouts)
                                                          layouts)
                                                        (cond-> (:sort-descending? @state-atom)
                                                          (reverse)))]
                                         (concat [(layout-comparison-text (layout-name layout))]
                                                 (for [column columns]
                                                   (on-click (fn []
                                                               (swap! state-atom
                                                                      assoc
                                                                      :selected-layout-rating-description (:layout-rating-description layout)
                                                                      :rating (:key column)
                                                                      :layout layout))
                                                             (cell (layouts/superimpose (assoc (visuals/rectangle-2 {:fill-color [100 100 100 255]})
                                                                                               :height 30
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
                                                                                        (layout-comparison-text (str (format "%.4f" (get (:summary layout)
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

(defn layout-comparison-view [named-layout-atoms statistics]
  (let [named-layouts (for [named-layout-atom named-layout-atoms]
                        (assoc (named-layout-atom-to-named-layout named-layout-atom)
                               :layout-rating-description (describe-layout-rating statistics
                                                                                  @(:layout-atom named-layout-atom))))
        cocoa-key-code-to-characters (map layout-to-cocoa-key-code-to-character (map :layout named-layouts))
        character-to-cocoa-key-codes (map layout-to-character-to-cocoa-key-code (map :layout named-layouts))]
    (layouts/superimpose (visuals/rectangle-2 {:fill-color [0 0 0 255]})
                         (layouts/vertically-2 {:margin 10}
                                               (layouts/with-margins 50 0 0 50 [layout-rating-comparison-view statistics named-layouts])
                                               (layouts/flow (layouts/with-margin 40
                                                               (layouts/vertically-2 {:margin 10}
                                                                                     (layout-comparison-text "editor")
                                                                                     [layout-editor
                                                                                      (:layout-atom (first named-layout-atoms))
                                                                                      {}
                                                                                      statistics]
                                                                                     [layout-editor
                                                                                      (:layout-atom (second named-layout-atoms))
                                                                                      (into {}
                                                                                            (for [differing-cocoa-keycode (map first (set/difference (set (second cocoa-key-code-to-characters))
                                                                                                                                                     (set (first cocoa-key-code-to-characters))))]
                                                                                              [differing-cocoa-keycode [100 150 100 255]]))
                                                                                      statistics]))

                                                             (layouts/with-margin 40
                                                               (layouts/vertically-2 {:margin 0}
                                                                                     (layout-comparison-text "heatmap")
                                                                                     [key-heat-map-view
                                                                                      (first cocoa-key-code-to-characters)
                                                                                      (first character-to-cocoa-key-codes)
                                                                                      (:character-distribution statistics)]
                                                                                     ;; (layout-comparison-text "")
                                                                                     [key-heat-map-view
                                                                                      (second cocoa-key-code-to-characters)
                                                                                      (second character-to-cocoa-key-codes)
                                                                                      (:character-distribution statistics)]))

                                                             (for [n-gram (map first (take 82 (reverse (sort-by second (:digram-distribution statistics)))))]
                                                               (layouts/with-margin 40
                                                                 (layouts/vertically-2 {:margin 10}
                                                                                       (ngram-view (first cocoa-key-code-to-characters)
                                                                                                   (first character-to-cocoa-key-codes)
                                                                                                   n-gram)
                                                                                       (ngram-view (second cocoa-key-code-to-characters)
                                                                                                   (second character-to-cocoa-key-codes)
                                                                                                   n-gram)))))))))

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



  (do (def named-layout-atom-1 (assoc (named-layout-to-named-layout-atom {:layout colemak-dh})
                                      :name "coleman-dh")
        #_(assoc (named-layout-to-named-layout-atom {:layout qwerty})
                 :name "qwerty")
        (named-layout-to-named-layout-atom (first @optimized-layouts-atom)))
      (def named-layout-atom-2 (named-layout-to-named-layout-atom (first @optimized-layouts-atom)))
      (start-view (fn []
                    [#'layout-comparison-view
                     [named-layout-atom-1
                      named-layout-atom-2]
                     #_hybrid-statistics
                     #_finnish-statistics
                     english-statistics])))

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
                                         )]
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

                             #_(optimize-layout text-statistics
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
