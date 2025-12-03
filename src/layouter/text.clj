(ns layouter.text
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [medley.core :as medley]))


(def english-characters ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                         "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(def finnish-characters (concat english-characters
                                ["ä" "å" "ö"]))

(def finnish-characters-without-å (concat english-characters
                                          ["ä" "ö"]))


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
   (text-statistics text finnish-characters-without-å))

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

(defonce finnish-statistics-without-å (assoc (text-statistics (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")
                                                              finnish-characters-without-å)
                                             :name "fi-å"))

(defonce hybrid-statistics (assoc (text-statistics (str (take 300000 (slurp "temp/text/kirjoja-ja-kirjailijoita.txt"))
                                                        (take 300000 (slurp "temp/text/the-hacker-crackdown.txt")))
                                                   finnish-characters)
                                  :name "hy"))

(defonce hybrid-statistics-without-å (assoc (text-statistics (str (apply str (take 300000 (slurp "temp/text/kirjoja-ja-kirjailijoita.txt")))
                                                                  (apply str (take 300000 (slurp "temp/text/the-hacker-crackdown.txt"))))
                                                             finnish-characters-without-å)
                                            :name "hy-å"))

(defonce wikinews-english-statistics (edn/read-string (slurp "temp/wikinews-english-statistics.edn")))
(defonce wikinews-finnish-statistics (edn/read-string (slurp "temp/wikinews-finnish-statistics.edn")))
(defonce wikinews-hybrid-statistics (edn/read-string (slurp "temp/wikinews-hybrid-statistics.edn")))
(defonce wikibooks-english-statistics (edn/read-string (slurp "temp/wikibooks-english-statistics.edn")))

;; (spit "temp/wikinews-english-statistics.edn" (pr-str (assoc (text/text-statistics (extract-english-text))
;;                                                             :name "wi-en")))
;;   (spit "temp/wikinews-finnish-statistics.edn" (pr-str (assoc (text/text-statistics (extract-finnish-text))
;;                                                               :name "wi-fi")))
;;   (spit "temp/wikinews-hybrid-statistics.edn" (pr-str (assoc (text/text-statistics (str (extract-english-text)
;;                                                                                         (extract-finnish-text)))
;;                                                               :name "wi-hy")))

(defn parse-keyboard-design-com-data-file [file-name]
  (->> (-> (slurp file-name)
           (string/split #"\n"))
       (map #(string/split % #"\t"))
       (drop 1)
       (map (fn [[character count]]
              [character (Integer/parseInt count)]))))

(defn parse-ngrams-from-keyboard-design-com-data-file-rows [data-rows]
  (->> data-rows
       (map (fn [[n-string count]]
              [(map str n-string) count]))
       (filter (fn [[n-gram _count]]
                 (every? (set finnish-characters-without-å)
                         n-gram)))))

(defn counts-to-propabilities [data-rows]
  (let [sum (reduce + (map second data-rows))]
    (map (fn [[value count]]
           [value (double (/ count sum))])
         data-rows)))

(defonce keyboard-design-com-english-text-statistics {:name "kdc"
                                                      :character-distribution (->> (parse-keyboard-design-com-data-file "temp/text/keyboard-design-com/english-frequency.txt")
                                                                                   (filter (comp (set finnish-characters-without-å) first))
                                                                                   (counts-to-propabilities)
                                                                                   (into {}))

                                                      :digram-distribution (->> (parse-keyboard-design-com-data-file "temp/text/keyboard-design-com/english-bigrams.txt")
                                                                                (parse-ngrams-from-keyboard-design-com-data-file-rows)
                                                                                (counts-to-propabilities)
                                                                                (select-probability-mass 0.95)
                                                                                (count))

                                                      :trigram-distribution (->> (parse-keyboard-design-com-data-file "temp/text/keyboard-design-com/english-trigrams.txt")
                                                                                 (parse-ngrams-from-keyboard-design-com-data-file-rows)
                                                                                 (counts-to-propabilities)
                                                                                 (select-probability-mass 0.95)
                                                                                 (count))})
