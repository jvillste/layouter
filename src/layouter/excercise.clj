(ns layouter.excercise
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]])
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


(defonce english-words (->> (io/resource "english-words.txt.zip")
                            (read-zipped-text-file)
                            (.getBytes)
                            (io/reader)
                            (line-seq)))

(defonce nykysuomensanalista (-> (io/resource "nykysuomensanalista.txt.zip")
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
                   (every? allowed-characters-set (seq word)))))))

(deftest test-filter-words-by-characters
  (is (= '("ab")
         (filter-words-by-characters ["a" "b"]
                                     ["abc"
                                      "ab"]))))

(defn excericse-word [number-of-characters statistics]
  (first (filter-words-by-characters (take-most-common-characters number-of-characters
                                                                  (:character-distribution statistics))
                                     (shuffle (concat english-words
                                                      nykysuomensanalista)))))
