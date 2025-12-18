(ns layouter.layout
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [layouter.keyboard :as keyboard]
   [medley.core :as medley])
  (:import
   java.util.Locale))

(defn layout-to-cocoa-key-code-to-character [layout]
  (medley/map-vals :character (medley/index-by :cocoa-key-code layout)))

(defn layout-to-character-to-cocoa-key-code [layout]
  (medley/map-vals :cocoa-key-code (medley/index-by :character layout)))

(defn layout-to-character-to-key [layout]
  (medley/map-vals keyboard/cocoa-key-code-to-key
                   (layout-to-character-to-cocoa-key-code layout)))

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

(def colemak-dh-with-qwerty-characters #{{:cocoa-key-code 0, :qwerty-character "a", :character "a"}
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

(def colemak-dh (set (map #(dissoc % :qwerty-character)
                          colemak-dh-with-qwerty-characters)))

(def dvorak-with-qwerty-characters #{{:cocoa-key-code 0, :qwerty-character "a", :character "a"}
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

(def dvorak (set (map #(dissoc % :qwerty-character)
                      dvorak-with-qwerty-characters)))


(def hand-alternating-named-layout {:text-statistics-name "kdc-en",
                                    :multipliers
                                    {:key-rating 1,
                                     :vertical-movement-in-skipgram 1,
                                     :vertical-movement 1,
                                     :trigram-roll 0.0,
                                     :hand-balance 0.1,
                                     :hand-alternation 1,
                                     :dist-from-colemak 0.0,
                                     :finger-type 0.1,
                                     :digram-roll 0.0,
                                     :horizontal-movement 1},
                                    :layout
                                    #{{:cocoa-key-code 17, :character "p"}
                                      {:cocoa-key-code 1, :character "s"}
                                      {:cocoa-key-code 37, :character "t"}
                                      {:cocoa-key-code 12, :character "l"}
                                      {:cocoa-key-code 11, :character "q"}
                                      {:cocoa-key-code 40, :character "i"}
                                      {:cocoa-key-code 8, :character "f"}
                                      {:cocoa-key-code 2, :character "d"}
                                      {:cocoa-key-code 6, :character "g"}
                                      {:cocoa-key-code 31, :character "k"}
                                      {:cocoa-key-code 9, :character "x"}
                                      {:cocoa-key-code 38, :character "e"}
                                      {:cocoa-key-code 16, :character ""}
                                      {:cocoa-key-code 4, :character "o"}
                                      {:cocoa-key-code 15, :character "c"}
                                      {:cocoa-key-code 32, :character "z"}
                                      {:cocoa-key-code 3, :character "n"}
                                      {:cocoa-key-code 13, :character "v"}
                                      {:cocoa-key-code 39, :character "u"}
                                      {:cocoa-key-code 46, :character "j"}
                                      {:cocoa-key-code 33, :character "ä"}
                                      {:cocoa-key-code 34, :character "y"}
                                      {:cocoa-key-code 45, :character "b"}
                                      {:cocoa-key-code 7, :character "w"}
                                      {:cocoa-key-code 14, :character "m"}
                                      {:cocoa-key-code 35, :character "ö"}
                                      {:cocoa-key-code 41, :character "a"}
                                      {:cocoa-key-code 0, :character "r"}
                                      {:cocoa-key-code 5, :character "h"}},
                                    :name "hand alternating"})

(def alternating-copy-paste-on-left {:layout #{{:cocoa-key-code 17, :character "p"}
                                               {:cocoa-key-code 46, :character "o"}
                                               {:cocoa-key-code 9, :character "b"}
                                               {:cocoa-key-code 7, :character "d"}
                                               {:cocoa-key-code 39, :character "ö"}
                                               {:cocoa-key-code 6, :character "g"}
                                               {:cocoa-key-code 2, :character "r"}
                                               {:cocoa-key-code 1, :character "c"}
                                               {:cocoa-key-code 14, :character "w"}
                                               {:cocoa-key-code 38, :character "e"}
                                               {:cocoa-key-code 35, :character ""}
                                               {:cocoa-key-code 16, :character "z"}
                                               {:cocoa-key-code 8, :character "n"}
                                               {:cocoa-key-code 34, :character "q"}
                                               {:cocoa-key-code 45, :character "f"}
                                               {:cocoa-key-code 33, :character "ä"}
                                               {:cocoa-key-code 15, :character "l"}
                                               {:cocoa-key-code 12, :character "v"}
                                               {:cocoa-key-code 32, :character "j"}
                                               {:cocoa-key-code 3, :character "s"}
                                               {:cocoa-key-code 41, :character "u"}
                                               {:cocoa-key-code 0, :character "m"}
                                               {:cocoa-key-code 40, :character "t"}
                                               {:cocoa-key-code 11, :character "k"}
                                               {:cocoa-key-code 4, :character "a"}
                                               {:cocoa-key-code 13, :character "x"}
                                               {:cocoa-key-code 31, :character "y"}
                                               {:cocoa-key-code 37, :character "i"}
                                               {:cocoa-key-code 5, :character "h"}}

                                     :multipliers {:key-rating 1,
                                                   :vertical-movement-in-skipgram 1,
                                                   :vertical-movement 1,
                                                   :trigram-roll 0.0,
                                                   :hand-balance 0.1,
                                                   :hand-alternation 1,
                                                   :distance-from-colemak 0.0,
                                                   :finger-type 1,
                                                   :digram-roll 0.0,
                                                   :horizontal-movement 1}})


(def alternating-oeitau {:text-statistics-name "kdc-en",
                         :multipliers
                         {:key-rating 1,
                          :vertical-movement-in-skipgram 1,
                          :vertical-movement 1,
                          :trigram-roll 0.0,
                          :hand-balance 0.1,
                          :hand-alternation 1,
                          :dist-from-colemak 0.0,
                          :finger-type 0.1,
                          :digram-roll 0.0,
                          :horizontal-movement 1},
                         :layout
                         #{{:cocoa-key-code 17, :character "p"}
                           {:cocoa-key-code 1, :character "s"}
                           {:cocoa-key-code 37, :character "t"}
                           {:cocoa-key-code 12, :character "l"}
                           {:cocoa-key-code 11, :character "q"}
                           {:cocoa-key-code 40, :character "i"}
                           {:cocoa-key-code 8, :character "f"}
                           {:cocoa-key-code 2, :character "d"}
                           {:cocoa-key-code 6, :character "g"}
                           {:cocoa-key-code 31, :character "k"}
                           {:cocoa-key-code 9, :character "x"}
                           {:cocoa-key-code 38, :character "e"}
                           {:cocoa-key-code 16, :character ""}
                           {:cocoa-key-code 4, :character "o"}
                           {:cocoa-key-code 15, :character "c"}
                           {:cocoa-key-code 32, :character "z"}
                           {:cocoa-key-code 3, :character "n"}
                           {:cocoa-key-code 13, :character "v"}
                           {:cocoa-key-code 39, :character "u"}
                           {:cocoa-key-code 46, :character "j"}
                           {:cocoa-key-code 33, :character "ä"}
                           {:cocoa-key-code 34, :character "y"}
                           {:cocoa-key-code 45, :character "b"}
                           {:cocoa-key-code 7, :character "w"}
                           {:cocoa-key-code 14, :character "m"}
                           {:cocoa-key-code 35, :character "ö"}
                           {:cocoa-key-code 41, :character "a"}
                           {:cocoa-key-code 0, :character "r"}
                           {:cocoa-key-code 5, :character "h"}},
                         :name "7"})

(def edited-alternating-oeitau {:text-statistics-name "kdc-en",
                                :multipliers
                                {:key-rating 1,
                                 :vertical-movement-in-skipgram 1,
                                 :vertical-movement 1,
                                 :trigram-roll 0.0,
                                 :hand-balance 0.1,
                                 :hand-alternation 1,
                                 :dist-from-colemak 0.0,
                                 :finger-type 0.1,
                                 :digram-roll 0.0,
                                 :horizontal-movement 1},
                                :layout
                                #{{:cocoa-key-code 17, :character "p"}
                                  {:cocoa-key-code 1, :character "s"}
                                  {:cocoa-key-code 37, :character "t"}
                                  {:cocoa-key-code 8, :character "l"}
                                  {:cocoa-key-code 11, :character "q"}
                                  {:cocoa-key-code 40, :character "i"}
                                  {:cocoa-key-code 2, :character "d"}
                                  {:cocoa-key-code 6, :character "g"}
                                  {:cocoa-key-code 31, :character "k"}
                                  {:cocoa-key-code 9, :character "x"}
                                  {:cocoa-key-code 38, :character "e"}
                                  {:cocoa-key-code 16, :character ""}
                                  {:cocoa-key-code 4, :character "o"}
                                  {:cocoa-key-code 15, :character "c"}
                                  {:cocoa-key-code 32, :character "z"}
                                  {:cocoa-key-code 3, :character "n"}
                                  {:cocoa-key-code 13, :character "v"}
                                  {:cocoa-key-code 39, :character "u"}
                                  {:cocoa-key-code 46, :character "j"}
                                  {:cocoa-key-code 33, :character "ä"}
                                  {:cocoa-key-code 34, :character "y"}
                                  {:cocoa-key-code 45, :character "b"}
                                  {:cocoa-key-code 14, :character "m"}
                                  {:cocoa-key-code 35, :character "ö"}
                                  {:cocoa-key-code 41, :character "a"}
                                  {:cocoa-key-code 7, :character "f"}
                                  {:cocoa-key-code 0, :character "r"}
                                  {:cocoa-key-code 5, :character "h"}
                                  {:cocoa-key-code 12, :character "w"}},
                                :name "7"})

(def edited-alternating-with-undo-optimization-oeitau {:text-statistics-name "kdc-en",
                                                       :multipliers
                                                       {:key-rating 1,
                                                        :vertical-movement-in-skipgram 1,
                                                        :vertical-movement 1,
                                                        :trigram-roll 0.0,
                                                        :hand-balance 0.1,
                                                        :hand-alternation 1,
                                                        :dist-from-colemak 0.0,
                                                        :finger-type 0.1,
                                                        :digram-roll 0.0,
                                                        :horizontal-movement 1},
                                                       :layout
                                                       #{{:cocoa-key-code 17, :character "p"}
                                                         {:cocoa-key-code 1, :character "s"}
                                                         {:cocoa-key-code 37, :character "t"}
                                                         {:cocoa-key-code 8, :character "l"}
                                                         {:cocoa-key-code 11, :character "z"}
                                                         {:cocoa-key-code 40, :character "i"}
                                                         {:cocoa-key-code 2, :character "d"}
                                                         {:cocoa-key-code 6, :character "g"}
                                                         {:cocoa-key-code 31, :character "k"}
                                                         {:cocoa-key-code 9, :character "x"}
                                                         {:cocoa-key-code 38, :character "e"}
                                                         {:cocoa-key-code 16, :character ""}
                                                         {:cocoa-key-code 4, :character "o"}
                                                         {:cocoa-key-code 15, :character "c"}
                                                         {:cocoa-key-code 32, :character "q"}
                                                         {:cocoa-key-code 3, :character "n"}
                                                         {:cocoa-key-code 13, :character "v"}
                                                         {:cocoa-key-code 39, :character "u"}
                                                         {:cocoa-key-code 46, :character "j"}
                                                         {:cocoa-key-code 33, :character "ä"}
                                                         {:cocoa-key-code 34, :character "y"}
                                                         {:cocoa-key-code 45, :character "b"}
                                                         {:cocoa-key-code 14, :character "m"}
                                                         {:cocoa-key-code 35, :character "ö"}
                                                         {:cocoa-key-code 41, :character "a"}
                                                         {:cocoa-key-code 7, :character "f"}
                                                         {:cocoa-key-code 0, :character "r"}
                                                         {:cocoa-key-code 5, :character "h"}
                                                         {:cocoa-key-code 12, :character "w"}},
                                                       :name "7"})



(defn replace-character [layout from-character to-character]
  (set (for [mapping layout]
         (if (= (:character mapping) from-character)
           (assoc mapping :character to-character)
           mapping))))

(deftest test-replace-character
  (is (= #{{:cocoa-key-code 1 :character "a"}
           {:cocoa-key-code 2 :character "c"}}
         (replace-character #{{:cocoa-key-code 1 :character "a"}
                              {:cocoa-key-code 2 :character "b"}}
                            "b"
                            "c"))))

(defn finnish-layout-to-finnish-layout-without-å [layout]
  (replace-character layout "å" ""))

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

(defn homerow-string [layout]
  (apply str (map (layout-to-cocoa-key-code-to-character layout) [0 1 2 3 5])))

(deftest test-homerow-string
  (is (= "asdfg"
         (homerow-string qwerty))))

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

(defn multipliers-to-short-layout-name [multipliers]
  (->> multipliers
       (sort-by first)
       (map second)
       (map (fn [value]
              (format-in-us-locale "%.1f" (float value))))
       (string/join " ")))

(defn layout-name [layout]
  (string/join "-"
               (remove nil?
                       [(when-some [name (:name layout)]
                          name)
                        #_(when-some [multipliers (:multipliers layout)]
                          #_(multipliers-to-layout-name multipliers)
                          (multipliers-to-short-layout-name multipliers))
                        (homerow-string (:layout layout))
                        (:text-statistics-name layout)])))

(defn number-of-differing-keys [layout-a layout-b]
  (count (set/difference layout-a layout-b)))

(defn layout-distance [layout-a layout-b]
  (double (/ (number-of-differing-keys layout-a layout-b)
             (count layout-a))))

(deftest test-layout-distance
  (is (= 0.0
         (layout-distance #{{:character "a", :cocoa-key-code 0}
                            {:character "s", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}}

                          #{{:character "a", :cocoa-key-code 0}
                            {:character "s", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}})))

  (is (= 0.5
         (layout-distance #{{:character "a", :cocoa-key-code 0}
                            {:character "s", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}}

                          #{{:character "s", :cocoa-key-code 0}
                            {:character "a", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}})))

  (is (= 1.0
         (layout-distance #{{:character "a", :cocoa-key-code 0}
                            {:character "s", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}}

                          #{{:character "f", :cocoa-key-code 0}
                            {:character "d", :cocoa-key-code 1}
                            {:character "s", :cocoa-key-code 2}
                            {:character "a", :cocoa-key-code 3}}))))

(defn layout-diversity [layouts]
  (let [pairs (pmap (fn [[a b]]
                      (layout-distance a b))
                    (for [a layouts
                          b layouts
                          :when (not= a b)]
                      [a b]))]
    (if (empty? pairs)
      0
      (/ (reduce + pairs)
         (count pairs)))))

(deftest test-layout-diversity
  (is (= 0
         (layout-diversity [#{{:character "a", :cocoa-key-code 0}
                              {:character "s", :cocoa-key-code 1}
                              {:character "d", :cocoa-key-code 2}
                              {:character "f", :cocoa-key-code 3}}

                            #{{:character "a", :cocoa-key-code 0}
                              {:character "s", :cocoa-key-code 1}
                              {:character "d", :cocoa-key-code 2}
                              {:character "f", :cocoa-key-code 3}}])))

  (is (= 1/2
         (layout-diversity [#{{:character "a", :cocoa-key-code 0}
                              {:character "s", :cocoa-key-code 1}
                              {:character "d", :cocoa-key-code 2}
                              {:character "f", :cocoa-key-code 3}}

                            #{{:character "a", :cocoa-key-code 0}
                              {:character "d", :cocoa-key-code 1}
                              {:character "s", :cocoa-key-code 2}
                              {:character "f", :cocoa-key-code 3}}])))

  (is (= 1
         (layout-diversity [#{{:character "a", :cocoa-key-code 0}
                              {:character "s", :cocoa-key-code 1}
                              {:character "d", :cocoa-key-code 2}
                              {:character "f", :cocoa-key-code 3}}

                            #{{:character "f", :cocoa-key-code 0}
                              {:character "d", :cocoa-key-code 1}
                              {:character "s", :cocoa-key-code 2}
                              {:character "a", :cocoa-key-code 3}}]))))

(defn layout-to-sequence [layout]
  (let [cocoa-key-code-to-character (layout-to-cocoa-key-code-to-character layout)]
    (->> keyboard/keyboard-keys
         (sort-by :cocoa-key-code)
         (map :cocoa-key-code)
         (map cocoa-key-code-to-character))))

(deftest test-layout-to-sequence
  (is (= '("a" "s" "d" "f" nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
         (layout-to-sequence #{{:character "a", :cocoa-key-code 0}
                               {:character "s", :cocoa-key-code 1}
                               {:character "d", :cocoa-key-code 2}
                               {:character "f", :cocoa-key-code 3}}))))

(defn layout-entropy [layouts]
  (let [number-of-layouts (count layouts)
        position-entropies (pmap (fn [characters-in-position]
                                   (let [frequencies (vals (frequencies characters-in-position))
                                         propabilities (map #(/ % number-of-layouts)
                                                            frequencies)]
                                     (- (reduce + (map #(* % (Math/log %))
                                                       propabilities)))))
                                 (apply pmap vector
                                        (pmap layout-to-sequence layouts)))]
    (/ (apply + position-entropies)
       (count position-entropies))))

(deftest test-layout-entropy
  (is (= -0.0
         (layout-entropy [#{{:character "a", :cocoa-key-code 0}
                            {:character "s", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}}

                          #{{:character "a", :cocoa-key-code 0}
                            {:character "s", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}}])))

  (is (= 0.08401784006787216
         (layout-entropy [#{{:character "a", :cocoa-key-code 0}
                            {:character "s", :cocoa-key-code 1}
                            {:character "d", :cocoa-key-code 2}
                            {:character "f", :cocoa-key-code 3}}

                          #{{:character "f", :cocoa-key-code 0}
                            {:character "d", :cocoa-key-code 1}
                            {:character "s", :cocoa-key-code 2}
                            {:character "a", :cocoa-key-code 3}}]))))

(defn variance [numbers]
  (let [mean (/ (reduce + numbers)
                (count numbers))]
    (/ (reduce + (map #(Math/pow (- % mean)
                                 2)
                      numbers))
       (count numbers))))

(deftest test-variance
  (is (= 0.0
         (variance [1 1 1])))

  (is (= 0.22222222222222224
         (variance [50 50 51])))

  (is (= 0.22222222222222224
         (variance [0 0 1])))

  (is (= 16.0
         (variance [0 0 0 0 100]))))

(defn number-diversity [numbers]
  (Math/sqrt (variance numbers)))

(deftest test-number-diversity
  (is (= 0.0
         (number-diversity [1 1 1])))

  (is (= 0.4714045207910317
         (number-diversity [1 1 2])))

  (is (= 0.4714045207910317
         (number-diversity [30 30 31])))

  (is (= 0.816496580927726
         (number-diversity [1 2 3])))

  (is (= 13.4412301024373
         (number-diversity [1 2 30])))

  (is (= 40.0
         (number-diversity [0 0 0 0 100]))))
