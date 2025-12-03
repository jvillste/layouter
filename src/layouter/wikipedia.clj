(ns layouter.wikipedia
  (:require
   [camel-snake-kebab.core :as camel-snake-kebab]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [clojure.xml :as xml]
   [jsonista.core :as jsonista]
   [layouter.text :as text]))

(defn parse-xml-string
  "Transforms xml string into clojure datastructures using clojure.xml."
  [xml-string]
  (xml/parse (io/input-stream (.getBytes xml-string))))

(defn read-json-string [string]
  (when string
    (jsonista/read-value (io/input-stream (.getBytes string
                                                     "UTF-8"))
                         (jsonista/object-mapper {:decode-key-fn camel-snake-kebab/->kebab-case-keyword}))))


(defn remove-date [news-text]
  (string/replace news-text #"\d+\. \w+ \d+\s+" ""))



(deftest test-remove-date
  (is (= "Ylen koirapentujen elämää kuvaavan"
         (remove-date "14. lokakuuta 2025 Ylen koirapentujen elämää kuvaavan"))))

(defn remove-english-date [news-text]
  (string/replace news-text #"^\w+, \w+ \d+, \d+\s+" ""))



(deftest test-remove-english-date
  (is (= "  Australia Related article"
         (remove-english-date "Monday, November 24, 2025   Australia Related article"))))


(defn transduce-reader
  "Apply transducer `xform` using reducing function `rf` and initial
   value `init` to all lines read from `reader`.

   Reads via .readLine in a loop, stops on nil."
  [xform rf ^java.io.BufferedReader reader]
  (let [rf* (xform rf)]
    (loop [acc (rf)]
      (let [line (.readLine reader)]
        (if (nil? line)
          ;; Finish the reduction
          (rf* acc)
          (let [acc' (rf* acc line)]
            (if (reduced? acc')
              @acc'
              (recur acc'))))))))

(defn transduce-zip-file-lines-2 [transducer reducer path]
  (with-open [gz-stream (java.util.zip.GZIPInputStream.
                         (io/input-stream path))
              reader   (io/reader gz-stream)]
    (transduce-reader transducer
               reducer
               reader)))

(defn transduce-pages [transducer reducer cirrus-zip-file-name]
  (transduce-zip-file-lines-2 (comp (map read-json-string)
                                    (filter :opening-text)
                                    (map :opening-text)
                                    transducer)
                              reducer
                              cirrus-zip-file-name))

(defn pages [cirrus-file-name]
  (->> (-> (slurp cirrus-file-name)
           (string/split #"\n"))
       (map read-json-string)
       (filter :opening-text)
       #_(sort-by :create-timestamp)
       #_(reverse)))

(defn extract-finnish-text []
  (->> (pages "/Users/jukka/Downloads/fiwikinews-20251124-cirrussearch-content.json")
       (sort-by :create-timestamp)
       (reverse)
       (take 280)
       #_(map :create-timestamp)
       (map :opening-text)
       (map remove-date)
       (apply str)
       (take 300000)
       (apply str)
       #_(count)
       #_(first)))

(defn extract-english-text []
  (->> (pages "/Users/jukka/Downloads/enwikinews-20251124-cirrussearch-content.json")
       (sort-by :create-timestamp)
       (reverse)
       (take 300)
       #_(map :create-timestamp)
       (map :opening-text)
       (map remove-english-date)
       (apply str)
       (take 300000)
       (apply str)
       #_(count)
       #_(first)
       ))

(defn transduce-zip-file-lines [transducer reducer path]
  (with-open [gz-stream (java.util.zip.GZIPInputStream.
                         (io/input-stream path))
              reader   (io/reader gz-stream)]
    (transduce transducer
               reducer
               (line-seq reader))))

(defn stream-gz-lines [path]
  (with-open [gz-stream (java.util.zip.GZIPInputStream.
                         (io/input-stream path))
              reader   (io/reader gz-stream)]
    (doseq [line (line-seq reader)]
      (println line))))


(comment



  (transduce-pages (take 1)
                   conj "/Users/jukka/Downloads/enwikibooks-20251124-cirrussearch-content.json.gz")

  (transduce-zip-file-lines-2 (comp (map read-json-string)
                                    (filter :wikibase-item)
                                    #_(drop-while #(not (= "English in Use" (:title %))))
                                    (map :title)
                                    (take 300))
                              conj
                              "/Users/jukka/Downloads/enwikibooks-20251124-cirrussearch-content.json.gz")

  (spit "temp/wikibooks-english-statistics.edn" (pr-str (assoc (->> (file-seq (io/file "/Users/jukka/google-drive/src/layouter/temp/text/english"))
                                                                   (filter (fn [file]
                                                                             (.isFile ^java.io.File file)))
                                                                   (map slurp)
                                                                   (apply str)
                                                                   (text/text-statistics))
                                                              :name "wb-en")))

  (spit "temp/wikinews-english-statistics.edn" (pr-str (assoc (text/text-statistics (extract-english-text))
                                                              :name "wn-en")))
  (spit "temp/wikinews-finnish-statistics.edn" (pr-str (assoc (text/text-statistics (extract-finnish-text))
                                                              :name "wn-fi")))
  (spit "temp/wikinews-hybrid-statistics.edn" (pr-str (assoc (text/text-statistics (str (extract-english-text)
                                                                                        (extract-finnish-text)))
                                                             :name "wn-hy")))


  (->> (pages "/Users/jukka/Downloads/enwikinews-20251124-cirrussearch-content.json")
       (sort-by :create-timestamp)
       (reverse)
       (take 50)
       #_(map :create-timestamp)
       (map :opening-text)
       (map #(subs % 0 200))
       #_(map remove-date)
       ;; (apply str)
       ;; (take 300000)
       ;; (apply str)
       #_(count)
       #_(first)
       )
  )
