(ns layouter.layout
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [medley.core :as medley])
  (:import
   java.util.Locale))

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

(defn homerow-string [cocoa-key-code-to-character]
  (apply str (map cocoa-key-code-to-character [0 1 2 3 5])))

(deftest test-homerow-string
  (is (= "asdfg"
         (homerow-string (layout-to-cocoa-key-code-to-character qwerty)))))

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

(defn layout-name [layout]
  (str (if-let [name  (:name layout)]
         name
         (multipliers-to-layout-name (:multipliers layout)))
       "-"
       (homerow-string (layout-to-cocoa-key-code-to-character (:layout layout)))
       "-"
       (:statistics-name layout)))
