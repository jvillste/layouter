(ns layouter.corpus
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]])
  (:import
   [fi.evident.raudikko Morphology]
   [java.util.zip ZipEntry ZipInputStream ZipOutputStream]
   [org.languagetool JLanguageTool Languages]))

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
  (with-open [zip (ZipOutputStream. (io/output-stream output-zip-path))]
    (.putNextEntry zip (ZipEntry. file-name-in-zip))
    (.write zip (.getBytes content "UTF-8"))
    (.closeEntry zip)))

(defonce finnish-words-sorted-set (->> (io/resource "nykysuomensanalista.txt.zip")
                                       (read-zipped-text-file)
                                       (.getBytes)
                                       (io/reader)
                                       (line-seq)
                                       (apply sorted-set)))

(defonce english-words-sorted-set (->> (io/resource "english-words.txt.zip")
                                       (read-zipped-text-file)
                                       (.getBytes)
                                       (io/reader)
                                       (line-seq)
                                       (apply sorted-set)))

(defn strings-with-prefix [a-sorted-set prefix]
  (subseq  a-sorted-set
           >= prefix
           <= (str prefix \uffff)))

(deftest test-strings-with-prefix
  (is (= '("hell" "hello")
         (strings-with-prefix (sorted-set "hello" "hell"  "halo")
                              "he"))))
(comment
  ;; run this to get nykysuomensanalista2024-vain-sanat.txt from
  ;; nykysuomensanalista2024.txt downloaded from
  ;; https://kotus.fi/sanakirjat/kielitoimiston-sanakirja/nykysuomen-sana-aineistot/nykysuomen-sanalista
  (->> (slurp "temp/nykysuomensanalista2024.txt")
       (.getBytes)
       (io/reader)
       (line-seq)
       (take 10)
       (map #(string/split % #"\t"))
       ;; (map first)
       ;; (map string/lower-case)
       ;; (distinct)
       ;; (string/join "\n")
       ;; (create-zip-from-string "resources/nykysuomensanalista.txt.zip"
       ;;                         "nykysuomensanalista.txt")
       )

  ;; run this to create english-words.txt.zip from master.txt form
  ;; https://github.com/jeremy-rifkin/Wordlist/blob/master/master.txt
  (->> (slurp "temp/master.txt")
       (.getBytes)
       (io/reader)
       (line-seq)
       (map string/lower-case)
       (distinct)
       (string/join "\n")
       (create-zip-from-string "resources/english-words.txt.zip"
                               "english-words.txt"))
  )

(def morphology (Morphology/loadBundled))

(defn finnish-word? [word]
  (or (and (not (= [] (.analyze (.newAnalyzer morphology)
                                word)))
           (not (= 1 (count word))))
      (and (< 2 (count word))
           (not (empty? (strings-with-prefix finnish-words-sorted-set
                                             word))))))

(deftest test-finnish-word?
  (is (finnish-word? "hiekkaakin"))
  (is (finnish-word? "hiek"))
  (is (not (finnish-word? "hi")))
  (is (not (finnish-word? "hiekkaakinx"))))

(def language-tool (JLanguageTool. (Languages/getLanguageForShortCode "en-US")))

(defn english-word? [word]
  (or (and (empty? (.check language-tool word))
           (or (contains? #{"i" "a"} word)
               (not (= 1 (count word)))))
      (and (< 2 (count word))
           (not (empty? (strings-with-prefix english-words-sorted-set
                                             word))))))
