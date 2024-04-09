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
   [medley.core :as medley]
   [medley.core :as meldey]
   [clojure.pprint :as pprint])
  (:import
   (java.util Random)))

;; KEYS

(def key-class-effort {:home 0
                       :easy-index 0.15
                       :regular 0.25
                       :sideways 0.55
                       :middle 1})

(def keyboard-keys [{:cocoa-key-code 0  :java-key-code 65       :finger 0 :class :home       :row 1 :column 0  :home-position? true  :qwerty-character "a"}
                    {:cocoa-key-code 1  :java-key-code 83       :finger 1 :class :home       :row 1 :column 1  :home-position? true  :qwerty-character "s"}
                    {:cocoa-key-code 2  :java-key-code 68       :finger 2 :class :home       :row 1 :column 2  :home-position? true  :qwerty-character "d"}
                    {:cocoa-key-code 3  :java-key-code 70       :finger 3 :class :home       :row 1 :column 3  :home-position? true  :qwerty-character "f"}
                    {:cocoa-key-code 4  :java-key-code 72       :finger 4 :class :regular    :row 1 :column 5  :home-position? false :qwerty-character "h"}
                    {:cocoa-key-code 5  :java-key-code 71       :finger 3 :class :regular    :row 1 :column 4  :home-position? false :qwerty-character "g"}
                    {:cocoa-key-code 6  :java-key-code 90       :finger 1 :class :regular    :row 2 :column 1  :home-position? false :qwerty-character "z"}
                    {:cocoa-key-code 7  :java-key-code 88       :finger 2 :class :regular    :row 2 :column 2  :home-position? false :qwerty-character "x"}
                    {:cocoa-key-code 8  :java-key-code 67       :finger 3 :class :easy-index :row 2 :column 3  :home-position? false :qwerty-character "c"}
                    {:cocoa-key-code 9  :java-key-code 86       :finger 3 :class :sideways   :row 2 :column 4  :home-position? false :qwerty-character "v"}
                    {:cocoa-key-code 11 :java-key-code 66       :finger 3 :class :middle     :row 2 :column 4  :home-position? false :qwerty-character "b"}
                    {:cocoa-key-code 12 :java-key-code 81       :finger 0 :class :regular    :row 0 :column 0  :home-position? false :qwerty-character "q"}
                    {:cocoa-key-code 13 :java-key-code 87       :finger 1 :class :sideways   :row 0 :column 0  :home-position? false :qwerty-character "w"}
                    {:cocoa-key-code 14 :java-key-code 69       :finger 1 :class :regular    :row 0 :column 1  :home-position? false :qwerty-character "e"}
                    {:cocoa-key-code 15 :java-key-code 82       :finger 2 :class :regular    :row 0 :column 2  :home-position? false :qwerty-character "r"}
                    {:cocoa-key-code 16 :java-key-code 89       :finger 4 :class :middle     :row 0 :column 5  :home-position? false :qwerty-character "y"}
                    {:cocoa-key-code 17 :java-key-code 84       :finger 3 :class :regular    :row 0 :column 3  :home-position? false :qwerty-character "t"}
                    {:cocoa-key-code 31 :java-key-code 79       :finger 6 :class :regular    :row 0 :column 8  :home-position? false :qwerty-character "o"}
                    {:cocoa-key-code 32 :java-key-code 85       :finger 4 :class :regular    :row 0 :column 6  :home-position? false :qwerty-character "u"}
                    {:cocoa-key-code 33 :java-key-code 16777445 :finger 7 :class :regular    :row 0 :column 10 :home-position? false :qwerty-character "å"}
                    {:cocoa-key-code 34 :java-key-code 73       :finger 5 :class :regular    :row 0 :column 7  :home-position? false :qwerty-character "i"}
                    {:cocoa-key-code 35 :java-key-code 80       :finger 7 :class :regular    :row 0 :column 9  :home-position? false :qwerty-character "p"}
                    {:cocoa-key-code 37 :java-key-code 76       :finger 6 :class :home       :row 1 :column 8  :home-position? true  :qwerty-character "l"}
                    {:cocoa-key-code 38 :java-key-code 74       :finger 4 :class :home       :row 1 :column 6  :home-position? true  :qwerty-character "j"}
                    {:cocoa-key-code 39 :java-key-code 16777444 :finger 7 :class :regular    :row 1 :column 10 :home-position? false :qwerty-character "ä"}
                    {:cocoa-key-code 40 :java-key-code 75       :finger 5 :class :home       :row 1 :column 7  :home-position? true  :qwerty-character "k"}
                    {:cocoa-key-code 41 :java-key-code 16777462 :finger 7 :class :home       :row 1 :column 9  :home-position? true  :qwerty-character "ö"}
                    {:cocoa-key-code 43 :java-key-code 44       :finger 5 :class :regular    :row 2 :column 7  :home-position? false :qwerty-character ","}
                    {:cocoa-key-code 44 :java-key-code 47       :finger 7 :class :regular    :row 2 :column 9  :home-position? false :qwerty-character "-"}
                    {:cocoa-key-code 45 :java-key-code 78       :finger 3 :class :sideways   :row 2 :column 5  :home-position? false :qwerty-character "n"}
                    {:cocoa-key-code 46 :java-key-code 77       :finger 4 :class :easy-index :row 2 :column 6  :home-position? false :qwerty-character "m"}
                    {:cocoa-key-code 47 :java-key-code 46       :finger 6 :class :regular    :row 2 :column 8  :home-position? false :qwerty-character "."}
                    {:cocoa-key-code 50 :java-key-code 192      :finger 0 :class :regular    :row 2 :column 0 :home-position? false :qwerty-character "<"}])


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

(def disabled-layout-characters #{"," "." "-" "<" "ö" "ä" "å"})
(def qwerty-characters-for-disabled-keys #{"w" "y" "b"})

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
                                   :d 0.4}))))

(defn filter-target-text [text]
  (apply str (filter (conj layout-characters " ")
                     (map str (string/lower-case text)))))

(defn filter-target-text-without-space [text]
  (apply str
         (filter layout-characters
                 (map str (string/lower-case text)))))

(defn normalized-n-gram-distribution [n text]
  (->> text
       filter-target-text
       (n-gram-distribution n)
       normalize-distribution))

(defn normalized-digram-distribution [text]
  (normalized-n-gram-distribution 2 text))

(defn normalized-trigram-distribution [text]
  (->> text
       filter-target-text
       (n-gram-distribution 3)
       normalize-distribution))

(defn normalized-character-distribution [text]
  (->> text
       filter-target-text
       character-distribution
       normalize-distribution))

(defn text-statistics [text]
  {:digram-distribution (select-probability-mass 0.95
                                                 (normalized-n-gram-distribution 2 text))

   :trigram-distribution (select-probability-mass 0.50
                                                  (normalized-n-gram-distribution 3 text))

   :character-distribution (normalized-character-distribution text)})


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

(defn multiply-effort [multiplier rating]
  (update rating
          :effort
          (partial * multiplier)))

(defn key-rating [key]
  {:rating :key
   :label (:class key)
   :effort (key-class-effort (:class key))})

(def ^:dynamic key-rating-multiplier 1)
(def ^:dynamic finger-type-multiplier 1)

(defn rate-key [key]
  [(multiply-effort finger-type-multiplier (rate-finger-type key))
   (multiply-effort key-rating-multiplier (key-rating key))])

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


(def ^:dynamic digram-roll-mulptiplier 1)
(def ^:dynamic trigram-roll-mulptiplier 1)
(def ^:dynamic horizontal-movement-multiplier 1)
(def ^:dynamic vertical-movement-multiplier 1)

(defn rate-key-pair [key-pair]
  [(multiply-effort vertical-movement-multiplier (rate-vertical-movement key-pair))
   (multiply-effort horizontal-movement-multiplier (rate-horizontal-movement key-pair))
   (multiply-effort digram-roll-mulptiplier (rate-n-gram-roll key-pair))])

(defn rate-key-triple [key-triple]
  [(multiply-effort trigram-roll-mulptiplier (rate-n-gram-roll key-triple))])

(defn rate-distribution [rate distribution]
  (reduce +
          (for [[value propability] distribution]
            (* propability
               (rate value)))))

(defn total-effort [ratings]
  (apply + (map :effort ratings)))

(defn rate-layout [text-statistics layout]
  (let [character-to-key (comp cocoa-key-code-to-key
                               (layout-to-character-to-cocoa-key-code layout))]
    (+ (rate-distribution (fn [digram]
                            (total-effort (rate-key-pair (map character-to-key digram))))
                          (:digram-distribution text-statistics))
       (rate-distribution (fn [trigram]
                            (total-effort (rate-key-triple (map character-to-key trigram))))
                          (:trigram-distribution text-statistics))
       (rate-distribution (fn [character]
                            (total-effort (rate-key (character-to-key character))))
                          (:character-distribution text-statistics)))))

(deftest test-rate-layout
  (is (= 1.8208333333333333
         (rate-layout (text-statistics "hello")
                      qwerty)))

  (is (= 0.9833333333333333
         (rate-layout (text-statistics "hello")
                      (layout-from-qwerty {"h" "k"
                                           "e" "j"
                                           "l" "d"
                                           "o" "f"})))))

(defn describe-layout-rating [text-statistics layout]
  (let [character-to-key (comp cocoa-key-code-to-key
                               (layout-to-character-to-cocoa-key-code layout))]
    {:digrams (for [digram-propability (->> (:digram-distribution text-statistics)
                                            (sort-by second)
                                            (reverse))]
                {:propability digram-propability
                 :ratings (rate-key-pair (map character-to-key
                                              (first digram-propability)))})
     :trigrams (for [trigram-propability (->> (:trigram-distribution text-statistics)
                                              (sort-by second)
                                              (reverse))]
                 {:propability trigram-propability
                  :ratings (rate-key-triple (map character-to-key
                                                 (first trigram-propability)))})
     :characters (for [character-propability (->> (:character-distribution text-statistics)
                                                  (sort-by second)
                                                  (reverse))]
                   {:propability character-propability
                    :ratings (rate-key (character-to-key (first character-propability)))})}))

(deftest test-describe-layout-rating
  (is (= '{:digrams
           ({:propability [("e" "l") 0.25],
             :ratings
             [{:label :different-hand, :effort 0, :rating :vertical-movement}
              {:label :different-finger,
               :effort 0,
               :rating :horizontal-movement}
              {:rating :roll, :effort 1, :label :no-roll}]}
            {:propability [("l" "l") 0.25],
             :ratings
             [{:label :same-row, :effort 0, :rating :vertical-movement}
              {:label :same-column, :effort 0, :rating :horizontal-movement}
              {:rating :roll, :effort 1, :label :no-roll}]}
            {:propability [("l" "o") 0.25],
             :ratings
             [{:label :same-finger-one-row-leap,
               :effort 0.75,
               :rating :vertical-movement}
              {:label :same-column, :effort 0, :rating :horizontal-movement}
              {:rating :roll, :effort 1, :label :no-roll}]}),
           :trigrams
           ({:propability [("l" "l" "o") 0.3333333333333333],
             :ratings [{:rating :roll, :effort 1, :label :no-roll}]}),
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
              {:rating :key, :label :regular, :effort 0.25}]})}
         (describe-layout-rating (text-statistics "hello")
                                 qwerty)))

  (is (= '{:digrams
           ({:propability [("e" "l") 0.25],
             :ratings
             [{:label :different-hand, :effort 0, :rating :vertical-movement}
              {:label :different-finger,
               :effort 0,
               :rating :horizontal-movement}
              {:rating :roll, :effort 1, :label :no-roll}]}
            {:propability [("l" "l") 0.25],
             :ratings
             [{:label :same-row, :effort 0, :rating :vertical-movement}
              {:label :same-column, :effort 0, :rating :horizontal-movement}
              {:rating :roll, :effort 1, :label :no-roll}]}
            {:propability [("l" "o") 0.25],
             :ratings
             [{:label :same-row, :effort 0, :rating :vertical-movement}
              {:label :different-finger,
               :effort 0,
               :rating :horizontal-movement}
              {:rating :roll,
               :effort 0.0,
               :label :adjacent-invards-home-row-roll}]}),
           :trigrams
           ({:propability [("l" "l" "o") 0.3333333333333333],
             :ratings [{:rating :roll, :effort 1, :label :no-roll}]}),
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
              {:rating :key, :label :home, :effort 0}]})}
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
                                 rating-description)]
    (assoc summary :total (->> (vals summary)
                               (map (fn [ratings]
                                      (reduce + (vals ratings))))
                               (reduce +)))))

(deftest test-summarize-rating-description
  (is (= {:digrams
          {:vertical-movement 0.0, :horizontal-movement 0.0, :roll -0.5},
          :characters {:finger-type -0.15000000000000002, :key 0.5},
          :total -0.15000000000000002}
         (summarize-rating-description '{:digrams
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
                                            {:label :no-roll, :effort -1, :rating :roll}]}
                                          {:propability [["l" "o"] 0.25],
                                           :ratings
                                           [{:label :same-row, :effort 0, :rating :vertical-movement}
                                            {:label :different-finger,
                                             :effort 0,
                                             :rating :horizontal-movement}
                                            {:label :invards-roll-on-home-row, :effort 0, :rating :roll}]}),
                                         :characters
                                         ({:propability ["l" 0.4],
                                           :ratings
                                           [{:effort -1/4, :label :middle, :rating :finger-type}
                                            {:rating :key, :label :home, :effort 0.5}]}
                                          {:propability ["o" 0.2],
                                           :ratings
                                           [{:effort 0, :label :index, :rating :finger-type}
                                            {:rating :key, :label :home, :effort 0.5}]}
                                          {:propability ["e" 0.2],
                                           :ratings
                                           [{:effort 0, :label :index, :rating :finger-type}
                                            {:rating :key, :label :home, :effort 0.5}]}
                                          {:propability ["h" 0.2],
                                           :ratings
                                           [{:effort -1/4, :label :middle, :rating :finger-type}
                                            {:rating :key, :label :home, :effort 0.5}]})}))))

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

(defn random-layout []
  (loop [remaining-cocoa-key-codes (sort (map :cocoa-key-code (remove (fn [key]
                                                                        (contains? qwerty-characters-for-disabled-keys
                                                                                   (:character key)))
                                                                      qwerty)))
         remaining-characters (filter (fn [character]
                                        (contains? layout-characters
                                                   character))
                                      (map :character qwerty))
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
                             :cocoa-key-code cocoa-key-code}))))))

(comment
  (sort-by :cocoa-key-code (random-layout))
  )

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

(defn gradient-descent-one [text-statistics layout]
  (assert (not (nil? layout)))
  (let [current-effort (rate-layout text-statistics layout)
        mappings-in-random-order (shuffle layout)
        {:keys [effort layout]} (->> (for [mapping-1 mappings-in-random-order
                                           mapping-2 mappings-in-random-order]
                                       (-> layout
                                           (disj mapping-1
                                                 mapping-2)
                                           (conj (assoc mapping-1 :cocoa-key-code (:cocoa-key-code mapping-2)))
                                           (conj (assoc mapping-2 :cocoa-key-code (:cocoa-key-code mapping-1)))))
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
             {:character "b", :qwerty-character "k"}
             {:character "c", :qwerty-character "j"}
             {:character "x", :qwerty-character "q"}
             {:character "y", :qwerty-character "w"}
             {:character "z", :qwerty-character "e"})
           (make-readable (gradient-descent-all (text-statistics "abc")
                                                #{{:character "a" :cocoa-key-code (qwerty-key-code "q")}
                                                  {:character "b" :cocoa-key-code (qwerty-key-code "w")}
                                                  {:character "c" :cocoa-key-code (qwerty-key-code "e")}
                                                  {:character "x" :cocoa-key-code (qwerty-key-code "f")}
                                                  {:character "y" :cocoa-key-code (qwerty-key-code "j")}
                                                  {:character "z" :cocoa-key-code (qwerty-key-code "k")}}))))

    (is (= '({:character "a", :qwerty-character "q"}
             {:character "b", :qwerty-character "j"}
             {:character "c", :qwerty-character "e"}
             {:character "x", :qwerty-character "f"}
             {:character "y", :qwerty-character "w"}
             {:character "z", :qwerty-character "k"})
           (make-readable
            (gradient-descent-one (text-statistics "abc")
                                  #{{:character "a" :cocoa-key-code (qwerty-key-code "q")}
                                    {:character "b" :cocoa-key-code (qwerty-key-code "w")}
                                    {:character "c" :cocoa-key-code (qwerty-key-code "e")}
                                    {:character "x" :cocoa-key-code (qwerty-key-code "f")}
                                    {:character "y" :cocoa-key-code (qwerty-key-code "j")}
                                    {:character "z" :cocoa-key-code (qwerty-key-code "k")}}))))))

(defn optimize-layout [text initial-layout generation-count on-ready]
  (let [text-statistics (text-statistics text)
        generation-size 50
        mutate-layout (fn [layout]
                        (nth (iterate mutate-layout layout)
                             (rand-int 3)))
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
                     [layout (rate-layout text-statistics layout)])]

      (let [crossbreeded-ratings (for [layout (map mutate-layout
                                                   (repeatedly generation-size
                                                               #(crossbreed-layouts (weighted-random ratings)
                                                                                    (weighted-random ratings))))]
                                   [layout (rate-layout text-statistics layout)])
            next-ratings (take-last generation-size
                                    (sort-by second
                                             (concat ratings
                                                     crossbreeded-ratings
                                                     (for [layout (repeatedly 5 random-layout)]
                                                       [layout (rate-layout text-statistics
                                                                            layout)]))))]

        (when (= 0 (mod generation-number 10))
          (prn) ;; TODO: remove me
          (prn 'generation-number generation-number) ;; TODO: remove me
          (prn 'ratings (take 10 (reverse (sort (map second next-ratings)))))) ;; TODO: remove me

        (if (< generation-number
               generation-count)
          (recur (inc generation-number)
                 next-ratings)
          (on-ready (last (sort-by (partial rate-layout text-statistics)
                                   (map (partial gradient-descent-all text-statistics)
                                        (map first (take-last 10 (sort-by second ratings))))))))))))



(def target-text #_"hello world"
  (filter-target-text (str (subs (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                 0 300000)
                           (subs (slurp "temp/text/the-hacker-crackdown.txt")
                                 0 300000))
                      #_(slurp "temp/text/kirjoja-ja-kirjailijoita.txt"
                               #_"temp/text/ga-fi.txt"
                               #_"temp/text/ga.txt")))

(comment
  (format "%.2f" (double 0.123455))


  (select-probability-mass 0.95
                           (normalized-trigram-distribution (slurp "temp/text/the-hacker-crackdown.txt")))
  (count (reverse (sort-by second (select-probability-mass 0.7 (normalized-trigram-distribution (slurp "temp/text/the-hacker-crackdown.txt"))))))
  ;; => 735
  ;; => 2764
  ;; => 7470
  ;; => 6133
  ) ;; TODO: remove me

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

(defn binding-multipliers [{:keys [digram-roll trigram-roll key-rating finger-type horizontal-movement vertical-movement]} function]
  (binding [digram-roll-mulptiplier digram-roll
            trigram-roll-mulptiplier trigram-roll
            key-rating-multiplier key-rating
            finger-type-multiplier finger-type
            horizontal-movement-multiplier horizontal-movement
            vertical-movement-multiplier vertical-movement]
    (function)))

(defn optimize-layout-with-multipliers [multipliers statistics]
  (binding-multipliers multipliers
                       (fn []
                         (gradient-descent-all statistics
                                               (random-layout)))))

(defn multipliers-to-layout-name [multipliers]
  (string/join ""
               (map str
                    (apply concat
                           (medley/map-keys (fn [key]
                                              (subs (name key)
                                                    0 1))
                                            multipliers)))))

(defn optimize-named-layout-with-multipliers [multipliers statistics statistics-name]
  {:name (str (multipliers-to-layout-name multipliers)
              statistics-name)
   :multipliers multipliers
   :layout (optimize-layout-with-multipliers multipliers
                                             statistics)})

(defonce optimized-layouts-atom (atom []))

(comment

  ;; (binding-multipliers {:digram-roll 0
  ;;                       :trigram-roll 0
  ;;                       :key-rating 2
  ;;                       :finger-type 0
  ;;                       :horizontal-movement 0
  ;;                       :vertical-movement 0}
  ;;                      (fn []
  ;;                       (gradient-descent-one english-statistics
  ;;                                             (:layout {:name "d0t0k2f0h0v0en",
  ;;                                                       :layout
  ;;                                                       #{{:character "e", :cocoa-key-code 40}
  ;;                                                         {:character "t", :cocoa-key-code 37}
  ;;                                                         {:character "u", :cocoa-key-code 35}
  ;;                                                         {:character "d", :cocoa-key-code 7}
  ;;                                                         {:character "g", :cocoa-key-code 5}
  ;;                                                         {:character "z", :cocoa-key-code 45}
  ;;                                                         {:character "a", :cocoa-key-code 0}
  ;;                                                         {:character "n", :cocoa-key-code 38}
  ;;                                                         {:character "v", :cocoa-key-code 34}
  ;;                                                         {:character "r", :cocoa-key-code 17}
  ;;                                                         {:character "l", :cocoa-key-code 50}
  ;;                                                         {:character "k", :cocoa-key-code 46}
  ;;                                                         {:character "y", :cocoa-key-code 39}
  ;;                                                         {:character "h", :cocoa-key-code 6}
  ;;                                                         {:character "f", :cocoa-key-code 33}
  ;;                                                         {:character "b", :cocoa-key-code 47}
  ;;                                                         {:character "i", :cocoa-key-code 2}
  ;;                                                         {:character "c", :cocoa-key-code 31}
  ;;                                                         {:character "x", :cocoa-key-code 44}
  ;;                                                         {:character "w", :cocoa-key-code 32}
  ;;                                                         {:character "s", :cocoa-key-code 3}
  ;;                                                         {:character "p", :cocoa-key-code 43}
  ;;                                                         {:character "m", :cocoa-key-code 4}
  ;;                                                         {:character "j", :cocoa-key-code 14}
  ;;                                                         {:character "o", :cocoa-key-code 41}
  ;;                                                         {:character "q", :cocoa-key-code 9}}}))))


  (reset! optimized-layouts-atom [])

  (do (def english-statistics (text-statistics (slurp "temp/text/the-hacker-crackdown.txt")))
      (def finnish-statistics (text-statistics (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")))
      (def hybrid-statistics (text-statistics (str (subs (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                         0 300000)
                                                   (subs (slurp "temp/text/the-hacker-crackdown.txt")
                                                         0 300000)))))

  (count (slurp "temp/text/kirjoja-ja-kirjailijoita.txt"))
  ;; => 324920

  (count (slurp "temp/text/the-hacker-crackdown.txt"))
  ;; => 663795

  (count (reverse (sort-by second (:trigram-distribution hybrid-statistics))))
  ;; => 428

  (count (reverse (sort-by second (:trigram-distribution (text-statistics (str #_(slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                                               (slurp "temp/text/the-hacker-crackdown.txt")))))))
  ;; => 443

  (def trigram-english (optimize-named-layout-with-multipliers {:digram-roll 1
                                                                :trigram-roll 1
                                                                :key-rating 1
                                                                :finger-type 1
                                                                :horizontal-movement 0.5
                                                                :vertical-movement 0.5}
                                                               english-statistics
                                                               "en"))

  (do
    (swap! optimized-layouts-atom
           conj
           (optimize-named-layout-with-multipliers {:digram-roll 0.5
                                                    :trigram-roll 0
                                                    :key-rating 2
                                                    :finger-type 0.2
                                                    :horizontal-movement 0.1
                                                    :vertical-movement 0}
                                                   english-statistics
                                                   "en"))
    (refresh-view!))



  (start-layout-comparison-view)

  (.start (Thread. (fn [] (def optimized-layouts-with-multipliers (doall (for [ ;; roll [0 1 2]
                                                                               ;; key-rating [0 1 2]
                                                                               ;; finger-type [0 1 2]
                                                                               ;; horizontal-movement [0 1 2]
                                                                               ;; vertical-movement [0 1 2]

                                                                               digram-roll [1]
                                                                               trigram-roll [1]
                                                                               key [0.5 1 2]
                                                                               movement [0.5 1 2]
                                                                               [statistics-name statistics] [[:en english-statistics] ;;[:fin finnish-statistics] [:hyb hybrid-statistics]
                                                                                                             ]]
                                                                           (let [multipliers {:digram-roll digram-roll
                                                                                              :trigram-roll trigram-roll
                                                                                              :key-rating key
                                                                                              :finger-type key
                                                                                              :horizontal-movement movement
                                                                                              :vertical-movement movement}]
                                                                             (do (prn multipliers statistics-name)
                                                                                 (optimize-named-layout-with-multipliers multipliers
                                                                                                                         statistics
                                                                                                                         statistics-name)))))
                            ))))

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

  ) ;; TODO: remove me

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


(comment
  (:column (cocoa-key-code-to-key ((layout-to-character-to-cocoa-key-code qwerty) "s")))
  ) ;; TODO: remove me

(defonce event-channel-atom (atom nil))

(defn start-view [view]
  (reset! event-channel-atom
          (application/start-application view
                                         :on-exit #(reset! event-channel-atom nil))))

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
                                                    (abs (int (rate-key (character-key character)))))]
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

(defn key-colors-for-fingers []
  (into {}
        (for [keyboard-key keyboard-keys]
          [(:cocoa-key-code keyboard-key)
           (get both-hands-finger-colors
                (:finger keyboard-key))])))

(def key-size 50)

(defn row-view [cocoa-key-codes cocoa-key-code-to-character key-color on-event]
  (layouts/horizontally-2 {:margin 1}
                          (for [cocoa-key-code cocoa-key-codes]
                            (let [character (cocoa-key-code-to-character cocoa-key-code)]
                              {:node (box (layouts/with-minimum-size key-size key-size (text character))
                                          {:fill-color (or (key-color cocoa-key-code)
                                                           [70 70 70 255])
                                           :padding 10})
                               :mouse-event-handler (fn [node event]
                                                      (when (= :nodes-under-mouse-changed (:type event))
                                                        (if (contains? (set (map :id (:nodes-under-mouse event)))
                                                                       (:id node))
                                                          (when on-event
                                                            (on-event {:type :mouse-entered-character
                                                                       :caracter character}))
                                                          (when on-event
                                                            (on-event {:type :mouse-left-character
                                                                       :caracter character}))))
                                                      (when (= :mouse-pressed (:type event))
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

(defn layout-editor [layout]
  (let [cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)]
    (fn [_layout]
      {:node [keyboard-view
              cocoa-key-code-to-character
              #_(key-colors-for-fingers)
              (key-colors-for-key-ratings)
              {:on-key-event (fn [event]
                               (when (= :mouse-pressed (:type event))
                                 )
                               (prn 'event event) ;; TODO: remove me

                               )}]
       :can-gain-focus? true
       :keyboard-event-handler (fn [_subtree event]
                                 (prn 'event event) ;; TODO: remove me
                                 )})))

(comment
  (start-view (fn []
                [#'layout-editor qwerty #_(first optimized-layouts)]))
  ) ;; TODO: remove me

(defn merge-summary [summary]
  (apply merge
         (select-keys summary [:total])
         (vals (dissoc summary :total))))

(deftest test-merge-summary
  (is (= {:total -0.7833712849797758,
          :vertical-movement -0.04221625526034797,
          :horizontal-movement -0.024670919809416138,
          :roll -0.708420818864572,
          :finger-type -0.27699373091857604,
          :key 0.2689304398731363}
         (merge-summary {:digrams
                         {:vertical-movement -0.04221625526034797,
                          :horizontal-movement -0.024670919809416138,
                          :roll -0.708420818864572},
                         :characters
                         {:finger-type -0.27699373091857604, :key 0.2689304398731363},
                         :total -0.7833712849797758}))))
(defn cell [content]
  (layouts/with-margin 10 (layouts/with-maximum-size nil 20 (layouts/center-vertically content))))

(defn rating-description-table [state-atom rating-to-aspect rating-description rating]
  (layouts/grid (let [aspect (rating-to-aspect rating)]
                  (concat [(map (fn [value]
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
                               (take 30 (get rating-description aspect)))))))

(defn rating-description-view [layout _rating rating-description]
  (let [state-atom (dependable-atom/atom {:highlighted-characters #{}})
        cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character (:layout layout))
        character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code (:layout layout))
        key-colors-for-fingers (key-colors-for-fingers)
        rating-to-aspect (into {} (for [aspect (keys rating-description)
                                        rating (map :rating (:ratings (first (get rating-description aspect))))]
                                    [rating aspect]))]
    (fn [_layout rating rating-description]
      (let [key-color (into {}
                            (concat (for [highlighted-character (:highlighted-characters @state-atom)]
                                      [(character-to-cocoa-key-code highlighted-character) [100 150 70 255]])))]
        (layouts/vertically-2 {:margin 10}
                              (text (:name layout))
                              [keyboard-view cocoa-key-code-to-character
                               (merge key-colors-for-fingers
                                      key-color)]
                              [rating-description-table state-atom rating-to-aspect rating-description rating])))))

(comment
  (start-view (fn [] [rating-description-view {:name :random
                                               :layout (random-layout)}
                      :vertical-movement
                      '{:digrams
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
                           {:rating :key, :label :regular :effort 0.5}]})}]))

  ) ;; TODO: remove me

(defn on-click [handler node]
  {:node node
   :mouse-event-handler (fn [_node event]
                          (when (= :mouse-clicked (:type event))
                            (prn 'click) ;; TODO: remove me
                            (handler))
                          event)})

(defn layout-rating-comparison-view []
  (let [state-atom (dependable-atom/atom {})]
    (fn []
      (let [layouts (for [layout (concat [ ;; {:name "qwerty"
                                          ;;  :layout qwerty}
                                          ;; trigram-english
                                          ;; {:name "random"
                                          ;;  :layout (random-layout)}
                                          ;; {:name "dvorak"
                                          ;;  :layout dvorak}
                                          {:name "colemak dh"
                                           :layout colemak-dh}]
                                         @optimized-layouts-atom
                                         ;;optimized-layouts-with-multipliers
                                         )]
                      (assoc layout :layout-rating-description (describe-layout-rating english-statistics
                                                                                       (:layout layout))))]
        (if-let [rating-description (:rating-description @state-atom)]
          (on-click (fn []
                      (swap! state-atom dissoc :rating-description))
                    [rating-description-view
                     (:layout @state-atom)
                     (:rating @state-atom)
                     rating-description])
          (let [layouts (for [layout layouts]
                          (assoc layout :summary (merge-summary (summarize-rating-description (:layout-rating-description layout)))))
                columns (for [column (into #{} (apply concat (map keys (map :summary layouts))))]
                          {:key column
                           :minimum (apply min (map column (map :summary layouts)))
                           :maximum (apply max (map column (map :summary layouts)))})]
            [layouts/grid (concat [(concat [(visuals/text "layout")]
                                           (for [column columns]
                                             (on-click (fn []
                                                         (if (= column (:sort-column @state-atom))
                                                           (swap! state-atom update :sort-descending? not)
                                                           (swap! state-atom assoc :sort-column column)))
                                                       (cell (visuals/text (name (:key column)))))))]
                                  (for [layout (-> (sort-by (fn [layout]
                                                              (get (:summary layout)
                                                                   (:key (or (:sort-column @state-atom)
                                                                             (first columns)))))
                                                            layouts)
                                                   (cond-> (:sort-descending? @state-atom)
                                                     (reverse)))]
                                    (concat [(visuals/text (:name layout))]
                                            (for [column columns]
                                              (on-click (fn []
                                                          (swap! state-atom assoc :rating-description (:layout-rating-description layout)
                                                                 :rating (:key column)
                                                                 :layout layout))
                                                        (cell (assoc (visuals/rectangle-2 {:fill-color [100 100 100 255]})
                                                                     :height 30
                                                                     :width (* 300
                                                                               (abs (/ (get (:summary layout)
                                                                                            (:key column))
                                                                                       (:maximum column))))
                                                                     #_(let [offset (* 0.99 (:minimum column))]
                                                                         (* 300
                                                                            (abs (/ (- (get (:summary layout)
                                                                                            (:key column))
                                                                                       offset)
                                                                                    (- (:maximum column)
                                                                                       offset))))))))))))]))))))

(defn start-layout-comparison-view []
  (start-view (fn []
                [#'layout-rating-comparison-view])))
(comment
  (start-layout-comparison-view)
  )

(defn digram-test-view [layout _digram-distribution _highlighted-characters]
  (let [state-atom (dependable-atom/atom {:highlighted-characters #{}})
        cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)
        character-to-cocoa-key-code (layout-to-character-to-cocoa-key-code layout)
        key-colors-for-fingers (key-colors-for-fingers cocoa-key-code-to-character)]
    (fn [layout digram-distribution highlighted-characters]
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
                                            (float (rate-layout digram-distribution
                                                                layout))))
                              (digram-view digram-distribution
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
        layout-rating (rate-layout (normalized-digram-distribution text)
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


(defn start-optimization [state-atom target-text]
  (.start (Thread. (fn []
                     (optimize-layout (filter-target-text target-text)
                                      (last (:layouts @state-atom))
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
      (let [state @state-atom
            digram-distribution (normalized-digram-distribution target-text)]
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
                                                                digram-distribution
                                                                #{}]]
                                                              (for [[previous-layout layout] (partition 2 1 (:layouts state))]
                                                                [digram-test-view
                                                                 layout
                                                                 digram-distribution
                                                                 (set/difference layout-characters
                                                                                 (set (map :character (set/intersection previous-layout
                                                                                                                        layout))))])))
                              [#'text-test-view
                               (last (:layouts state))
                               (subs target-text 0 100)])))))

(comment
  (start-view (fn [] [#'optimization-view "kotivara vetää myynnistä lisää makkaroita"]))
  (start-view (fn [] [#'optimization-view target-text]))
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
  (reset! event-channel-atom
          (application/start-application (fn []
                                           [#'optimization-view target-text])
                                         ;; #'digram-distribution-comparison-view
                                         :on-exit #(reset! event-channel-atom nil))))

(defn refresh-view! []
  (when @event-channel-atom
    (async/>!! @event-channel-atom
               {:type :redraw})))

(refresh-view!)

(comment

  (spit "optimized-layout-for-finnish-and-english.edn" (pr-str optimized-layout))
  (spit "qwerty.edn" (pr-str qwerty))
  (def optimized-layout (edn/read-string (slurp "optimized-layout.edn")))
  ) ;; TODO: remove me
