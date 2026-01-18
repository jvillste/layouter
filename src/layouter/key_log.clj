(ns layouter.key-log
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [com.stuartsierra.frequencies :as frequencies]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [layouter.corpus :as corpus]
   [layouter.gui :as gui]
   [layouter.layout :as layout]
   [layouter.text :as text]
   [layouter.view :as view]
   [medley.core :as medley]))



(defn transduce-reader [xform rf ^java.io.BufferedReader reader]
  (let [rf* (xform rf)]
    (loop [acc (rf)]
      (let [character (.read reader)]
        (if (= -1 character)
          (rf* acc)
          (let [acc' (rf* acc character)]
            (if (reduced? acc')
              @acc'
              (recur acc'))))))))

(defn parse-key-log-from-file [file-name]
  (with-open [reader (io/reader file-name)]
    (transduce-reader (comp (map char)
                            (partition-by #{\,})
                            (remove #{[\,]})
                            (map (partial apply str))
                            (map string/trim)
                            (map (fn [line]
                                   (map parse-long (string/split line #" "))))
                            (map (fn [[keycode time]]
                                   {:keycode keycode
                                    :time time})))
                      conj
                      reader)))

(def key-log-file-path "/Users/jukka/nitor-src/posti/matching/temp/data/keylog.txt")

(def space-cocoa-key-code 49)
(def back-space-cocoa-key-code 51)
(def second-in-microseconds 1000000)

(defn apply-back-spaces [events]
  (reduce (fn [events event]
            (if (= (:keycode event) back-space-cocoa-key-code)
              (if (empty? events)
                events
                (pop events))
              (conj events event)))
          []
          events))

(deftest test-apply-back-spaces
  (is (= (apply-back-spaces [{:keycode 1}
                             {:keycode 2}
                             {:keycode 3}])

         (apply-back-spaces [{:keycode 1}
                             {:keycode 5}
                             {:keycode 6}
                             {:keycode back-space-cocoa-key-code}
                             {:keycode back-space-cocoa-key-code}
                             {:keycode 2}
                             {:keycode 3}]))))

(defn key-log-to-string [parsed-log minimum-pause-for-inserting-space]
  (->> parsed-log
       #_(sort-by :time)
       (apply-back-spaces)
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
       (map (->> (assoc (layout/layout-to-cocoa-key-code-to-character layout/qwerty)
                        space-cocoa-key-code " "
                        back-space-cocoa-key-code " ")
                 (medley/map-vals #(get {"," " "
                                         "." " "
                                         "-" " "
                                         "<" " "} % %))
                 (medley/remove-vals #{"," "." "-" "<"})))
       (string/join "")))

(deftest test-key-log-to-string
  (is (= "ab c"
         (key-log-to-string (let [character-to-cocoa-key-code (layout/layout-to-character-to-cocoa-key-code layout/qwerty)]
                              [{:keycode (character-to-cocoa-key-code "a"), :time 0}
                               {:keycode (character-to-cocoa-key-code "b"), :time 1}
                               {:keycode (character-to-cocoa-key-code "c"), :time 3}])
                            2))))

(def ^:private word-classifications-file-name "temp/word-clsasifications.edn")

(defonce word-classifications-atom (dependable-atom/atom (if (.exists (io/file word-classifications-file-name))
                                                           (edn/read-string (slurp word-classifications-file-name))
                                                           {})))

;; hot-right-now TODO: remove me
(def ^:private statistics-from-key-log-file-name "temp/statistics-from-key-log.edn")

(defonce statistics-from-key-log (if (.exists (io/file statistics-from-key-log-file-name))
                                   (edn/read-string (slurp statistics-from-key-log-file-name))
                                   nil))

(defn word-language [word]
  (let [classification (get @word-classifications-atom word)]
    (if (contains? #{:fi :en} classification)
      classification
      (cond (corpus/finnish-word? word)
            :fi

            (corpus/english-word? word)
            :en

            :else
            nil))))

(defn key-log-to-string-from-file [file-name]
  (-> (parse-key-log-from-file file-name)
      (key-log-to-string (* 2 second-in-microseconds))))

(defn key-log-to-words-from-file [file-name]
  (-> (key-log-to-string-from-file file-name)
      (string/split #"\s+")))

(defn key-log-to-words-with-language-from-file [file-name]
  (->> (key-log-to-words-from-file file-name)
       (pmap (fn [word]
               {:word word
                :language (or (word-language word)
                              (get @word-classifications-atom word))}))
       #_(filter :language)))

(defn filter-known-words [words]
  (let [english-word-set (set (filter #(< 2 (count %))
                                      corpus/english-words-sorted-set))]
    #_(map (fn [word]
             [(contains? english-word-set word)
              (corpus/finnish-word? word)])
           words)
    (filter (fn [word]
              (or #_(contains? english-word-set word)
                  (corpus/finnish-word? word)
                  (corpus/english-word? word)))
            words)))

(deftest test-filter-known-words
  (is (= (filter-known-words ["bbb" "moi"]))))


;; TODO: keylogger logs characters that were not typed by me
;; keylogger should also log characters rather than keycodes





(comment
  (apply str (take-last 100 (slurp key-log-file-path)))
  (take-last 10 (key-log-from-file key-log-file-path))




  (time (count (filter-known-words (key-log-to-words-from-file key-log-file-path))))
  (text/text-statistics (string/join " " (filter-known-words (key-log-to-words-from-file key-log-file-path))))

  (text/text-statistics (string/join " " (->> (key-log-to-words-from-file key-log-file-path)
                                              (remove (comp #{1} count)))))

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


(comment

  (->> (key-log-to-words-with-language-from-file key-log-file-path)
       (group-by :language)
       (medley/map-vals (fn [words-with-langauge]
                          (string/join " " (map :word words-with-langauge)))))

  (def statistics-from-key-log (let [words-with-language (->> (key-log-to-words-from-file key-log-file-path)
                                                              (pmap (fn [word]
                                                                      {:word word
                                                                       :language (or (word-language word)
                                                                                     (get @word-classifications-atom word))}))
                                                              (filter :language))]
                                 (merge (->> words-with-language
                                             (group-by :language)
                                             (medley/map-vals (fn [classified-words]
                                                                (assoc (text/text-statistics (string/join " " (map :word classified-words)))
                                                                       :name (str "kl-" (name (:language (first classified-words))))))))
                                        {:hybrid (assoc (->> words-with-language
                                                             (filter (comp #{:en :fi} :language))
                                                             (map :word)
                                                             (string/join " ")
                                                             (text/text-statistics))
                                                        :name "kl-hy")})))

  (spit "temp/statistics-from-key-log.edn"
        (pr-str statistics-from-key-log))

  )





(defn unknown-words []
  (->> (key-log-to-words-from-file key-log-file-path)
       (distinct)
       (pmap (fn [word]
               {:word word
                :language (word-language word)}))
       (filter (comp nil? :language))
       (map :word)))

(comment

  (reset! word-classifications-atom (edn/read-string (slurp word-classifications-file-name)))

  (frequencies (vals @word-classifications-atom))


  (->> (unknown-words)
       (remove #(>= 2 (count %)))
       (frequencies)
       (sort-by second)
       (reverse)
       (take 10))

  )


(defn next-words-to-be-classified []
  (->> @word-classifications-atom
       (medley/filter-vals (comp not keyword?))
       (take 8)
       (into {})))

(defn word-classification-view []
  (let [state-atom (dependable-atom/atom {:current-word-classifications (next-words-to-be-classified)})]
    (fn []
      (let [current-word-classifications (:current-word-classifications @state-atom)
            character-to-word (into {} (map vector
                                            ["a" "s" "d" "f" "j" "k" "l" "ö"]
                                            (keys current-word-classifications)))]
        {:node (gui/black-background (layouts/center (layouts/vertically-2 {:margin 50}
                                                                           (for [[key word] character-to-word]
                                                                             (gui/text (str key ": " word)
                                                                                       {:color (case (get current-word-classifications
                                                                                                          word)
                                                                                                 :fi
                                                                                                 [0.5 0.5 1.0 1.0]

                                                                                                 :en
                                                                                                 [0.8 0.5 0.0 1.0]

                                                                                                 [0.7 0.7 0.7 1.0])})))))
         :can-gain-focus? true
         :keyboard-event-handler (fn [_subtree event]
                                   (when (= :key-pressed
                                            (:type event))

                                     (let [word (get character-to-word
                                                     (str (:character event)))]
                                       (when (some? word)
                                         (swap! state-atom
                                                update-in
                                                [:current-word-classifications word]
                                                (fn [class]
                                                  (case class
                                                    :fi :en
                                                    :en :invalid
                                                    :invalid :fi
                                                    :fi)))))

                                     (when (= :space (:key event))
                                       (swap! word-classifications-atom
                                              (fn [word-classifications]
                                                (merge word-classifications
                                                       (medley/map-vals (fn [class]
                                                                          (if (keyword? class)
                                                                            class
                                                                            :invalid))
                                                                        current-word-classifications))))
                                       (swap! state-atom assoc :current-word-classifications (next-words-to-be-classified)))))}))))
(comment
  ;; add unknown words to be classified
  (do (swap! word-classifications-atom
             (fn [word-classifications]
               (merge (->> (unknown-words)
                           #_(take 10)
                           (map (fn [word]
                                  [word nil]))
                           (into {}))
                      word-classifications)))
      nil)

  ;; classify
  (view/start-view #'word-classification-view)

  ;; save classifications
  (spit word-classifications-file-name (pr-str (medley/filter-vals keyword? @word-classifications-atom)))

  (medley/filter-vals some? @word-classifications-atom)

  (->> (key-log-to-string (parse-key-log-from-file key-log-file-path)
                          (* 2 second-in-microseconds))
       (take-last 100)
       (apply str))

  (->> (key-log-to-words-from-file key-log-file-path)
       (take-last 50)
       (string/join " "))

  (def text-per-language (->> (key-log-to-words-from-file key-log-file-path)
                              ;;       (take-last 500)
                              (pmap (fn [word]
                                      {:word word
                                       :language (or (word-language word)
                                                     (get @word-classifications-atom word))}))
                              (filter :language)
                              (group-by :language)
                              #_(medley/map-vals count)
                              (medley/map-vals (fn [classified-words]
                                                 (string/join " " (map :word classified-words))))))

  ;; => {:fi 5568, :en 4282, :invalid 1131}
  ;; => {:fi 4634, :en 3703, :invalid 1003}

  ;; => {:fi 4625, :en 3702, nil 1003}

  )

(view/hard-refresh-view!)

(def key-log-text-statistics '{:name "key-log with single letters removed"
                               :digram-distribution
                               {("e" "i") 0.003419035958826464,
                                ("c" "e") 0.002240058041989752,
                                ("w" "e") 0.001024803881558065,
                                ("o" "v") 9.613204552668571E-4,
                                ("g" "o") 5.350745930258922E-4,
                                ("a" "t") 0.01159932888949349,
                                ("h" "h") 0.001324082891216615,
                                ("l" "ö") 8.43422663583186E-4,
                                ("o" "k") 0.002711649208724437,
                                ("o" "e") 4.625221058359407E-4,
                                ("i" "s") 0.01003038135401079,
                                ("t" "a") 0.01509998639640865,
                                ("h" "i") 0.001695914388065116,
                                ("n" "s") 0.002666303904230717,
                                ("c" "t") 0.002566544234344534,
                                ("o" "g") 0.002022400580419898,
                                ("r" "j") 9.159751507731374E-4,
                                ("l" "f") 7.980773590894663E-4,
                                ("e" "r") 0.01251530404026663,
                                ("d" "o") 0.002158436493901057,
                                ("a" "i") 0.005640955879018728,
                                ("g" "l") 8.071464199882102E-4,
                                ("l" "i") 0.007436629936970027,
                                ("j" "e") 0.001342221013014102,
                                ("i" "ä") 4.625221058359407E-4,
                                ("r" "e") 0.01157212170679726,
                                ("p" "h") 0.001015734820659321,
                                ("n" "c") 0.001750328753457579,
                                ("ä" "s") 6.620414456083073E-4,
                                ("ä" "r") 5.894889584183558E-4,
                                ("e" "y") 0.001813812179748787,
                                ("r" "k") 7.527320545957466E-4,
                                ("l" "e") 0.008796989071781617,
                                ("r" "y") 0.002829547000408108,
                                ("i" "g") 0.00366390060309255,
                                ("f" "e") 0.00115177073414048,
                                ("y" "o") 0.001133632612342992,
                                ("i" "c") 0.001432911622001542,
                                ("y" "l") 9.613204552668571E-4,
                                ("h" "o") 0.002484922686255838,
                                ("s" "e") 0.009177889629528861,
                                ("o" "r") 0.007110143744615245,
                                ("u" "v") 4.806602276334286E-4,
                                ("i" "n") 0.01983403618555299,
                                ("s" "l") 7.164558110007709E-4,
                                ("r" "s") 0.002865823244003083,
                                ("s" "ä") 0.001541740352786469,
                                ("o" "d") 0.003255792862649073,
                                ("a" "k") 0.003047204461977962,
                                ("u" "o") 0.002131229311204825,
                                ("e" "p") 0.001305944769419127,
                                ("e" "l") 0.006185099532943364,
                                ("a" "d") 0.003337414410737768,
                                ("t" "h") 0.006874348161247903,
                                ("ä" "i") 0.001052011064254296,
                                ("b" "y") 4.715911667346846E-4,
                                ("b" "k") 4.534530449371968E-4,
                                ("r" "g") 6.529723847095633E-4,
                                ("o" "c") 7.436629936970027E-4,
                                ("e" "u") 5.169364712284043E-4,
                                ("s" "p") 9.069060898743935E-4,
                                ("p" "t") 0.001160839795039224,
                                ("d" "u") 0.001605223779077677,
                                ("j" "a") 0.004298734866004625,
                                ("v" "o") 0.001070149186051784,
                                ("l" "k") 0.001197116038634199,
                                ("ö" "l") 5.441436539246361E-4,
                                ("d" "s") 0.001052011064254296,
                                ("s" "o") 0.003700176846687526,
                                ("t" "ä") 0.006321135446424523,
                                ("o" "w") 0.003718314968485013,
                                ("w" "a") 8.162154808869542E-4,
                                ("b" "d") 0.001070149186051784,
                                ("b" "p") 5.441436539246361E-4,
                                ("z" "e") 7.436629936970027E-4,
                                ("p" "o") 0.001922640910533714,
                                ("l" "u") 0.002593751417040765,
                                ("e" "f") 0.002249127102888496,
                                ("w" "h") 8.796989071781617E-4,
                                ("e" "e") 0.003863419942864916,
                                ("n" "k") 0.002312610529179703,
                                ("c" "a") 0.002747925452319412,
                                ("g" "a") 6.801795674057951E-4,
                                ("u" "u") 0.002230988981091008,
                                ("o" "i") 0.005169364712284043,
                                ("s" "d") 4.897292885321725E-4,
                                ("t" "i") 0.01244275155307668,
                                ("g" "i") 0.001478256926495261,
                                ("b" "r") 8.524917244819299E-4,
                                ("u" "e") 0.00311068788826917,
                                ("s" "t") 0.01432004715911667,
                                ("i" "r") 0.002539337051648302,
                                ("j" "ä") 7.255248718995148E-4,
                                ("f" "r") 0.00161429283997642,
                                ("i" "l") 0.005659094000816215,
                                ("t" "o") 0.00991248356232712,
                                ("d" "ä") 4.534530449371968E-4,
                                ("t" "y") 0.001886364666938738,
                                ("g" "q") 0.001287806647621639,
                                ("m" "e") 0.007781254251122296,
                                ("l" "d") 7.980773590894663E-4,
                                ("a" "h") 7.345939327982587E-4,
                                ("h" "a") 0.004752187910941822,
                                ("o" "j") 0.001922640910533714,
                                ("n" "d") 0.004198975196118442,
                                ("c" "i") 9.975966988618329E-4,
                                ("s" "c") 0.001795674057951299,
                                ("d" "a") 0.003301138167142792,
                                ("ä" "m") 4.715911667346846E-4,
                                ("j" "s") 6.166961411145876E-4,
                                ("h" "y") 4.987983494309164E-4,
                                ("s" "u") 0.002784201695914388,
                                ("h" "e") 0.006185099532943364,
                                ("r" "t") 0.002439577381762119,
                                ("a" "m") 0.004053870221738539,
                                ("a" "g") 0.001650569083571396,
                                ("i" "h") 9.88527637963089E-4,
                                ("a" "e") 8.887679680769056E-4,
                                ("i" "o") 0.003863419942864916,
                                ("i" "i") 0.003990386795447331,
                                ("w" "i") 0.001695914388065116,
                                ("g" "r") 0.00115177073414048,
                                ("a" "a") 0.01152677640230354,
                                ("ä" "ä") 0.002847685122205596,
                                ("e" "q") 6.892486283045391E-4,
                                ("l" "t") 0.002883961365800571,
                                ("i" "t") 0.01035686754636557,
                                ("k" "y") 0.001333151952115358,
                                ("w" "s") 8.887679680769056E-4,
                                ("k" "k") 0.002938375731193035,
                                ("e" "d") 0.003981317734548587,
                                ("x" "t") 0.001260599464925407,
                                ("q" "u") 0.002775132635015644,
                                ("ä" "l") 9.341132725706253E-4,
                                ("n" "e") 0.005350745930258922,
                                ("n" "i") 0.004951707250714189,
                                ("c" "r") 0.001269668525824151,
                                ("u" "r") 0.002611889538838253,
                                ("e" "a") 0.004525461388473224,
                                ("u" "c") 0.00110642542964676,
                                ("e" "n") 0.01377590350519204,
                                ("i" "k") 0.003428105019725207,
                                ("a" "w") 7.345939327982587E-4,
                                ("b" "e") 0.00145104974379903,
                                ("e" "x") 0.002131229311204825,
                                ("p" "ä") 6.801795674057951E-4,
                                ("m" "a") 0.00663855257788056,
                                ("l" "m") 8.34353602684442E-4,
                                ("f" "s") 6.257652020133315E-4,
                                ("y" "t") 0.001977055275926178,
                                ("o" "l") 0.004135491769827234,
                                ("r" "i") 0.005241917199473994,
                                ("ä" "h") 4.534530449371968E-4,
                                ("i" "a") 0.002738856391420668,
                                ("n" "v") 5.985580193170997E-4,
                                ("n" "g") 0.004570806692966943,
                                ("i" "p") 0.002294472407382216,
                                ("e" "m") 0.00331927628894028,
                                ("a" "u") 0.001686845327166372,
                                ("t" "t") 0.008198431052464516,
                                ("e" "w") 9.250442116718814E-4,
                                ("f" "f") 0.001596154718178933,
                                ("k" "a") 0.005414229356550129,
                                ("u" "k") 0.001949848093229946,
                                ("t" "r") 0.00331927628894028,
                                ("n" "o") 0.005776991792499887,
                                ("m" "y") 4.625221058359407E-4,
                                ("a" "v") 0.001877295606039995,
                                ("n" "f") 4.715911667346846E-4,
                                ("o" "n") 0.01279644492812769,
                                ("w" "o") 0.001523602230988981,
                                ("y" "k") 5.169364712284043E-4,
                                ("u" "m") 0.00230354146828096,
                                ("a" "l") 0.005677232122613703,
                                ("n" "m") 5.350745930258922E-4,
                                ("a" "y") 0.001668707205368884,
                                ("ä" "n") 0.002285403346483472,
                                ("e" "v") 0.002367024894572167,
                                ("e" "k") 0.001251530404026663,
                                ("c" "h") 0.002430508320863375,
                                ("r" "c") 9.79458577064345E-4,
                                ("y" "n") 7.073867501020269E-4,
                                ("o" "s") 0.003836212760168685,
                                ("o" "h") 4.534530449371968E-4,
                                ("a" "s") 0.006520654786196889,
                                ("b" "o") 9.613204552668571E-4,
                                ("g" "h") 7.799392372919784E-4,
                                ("p" "l") 0.002829547000408108,
                                ("f" "a") 8.34353602684442E-4,
                                ("o" "t") 0.003818074638371197,
                                ("t" "ö") 5.894889584183558E-4,
                                ("ö" "y") 5.169364712284043E-4,
                                ("i" "v") 0.001632430961773908,
                                ("d" "d") 0.001378497256609078,
                                ("c" "u") 8.615607853806738E-4,
                                ("o" "p") 0.002503060808053326,
                                ("r" "o") 0.00728245590169138,
                                ("m" "o") 0.004879154763524237,
                                ("a" "n") 0.01167188137668344,
                                ("f" "t") 4.715911667346846E-4,
                                ("n" "p") 0.001097356368748016,
                                ("t" "k") 5.62281775722124E-4,
                                ("m" "i") 0.004960776311612932,
                                ("n" "n") 0.005260055321271482,
                                ("p" "e") 0.002902099487598059,
                                ("i" "z") 8.887679680769056E-4,
                                ("x" "p") 5.078674103296604E-4,
                                ("s" "y") 0.001033872942456809,
                                ("k" "i") 0.003536933750510135,
                                ("b" "c") 9.703895161656011E-4,
                                ("u" "l") 0.004117353648029747,
                                ("r" "r") 0.00106108012515304,
                                ("n" "t") 0.007345939327982587,
                                ("y" "ö") 6.892486283045391E-4,
                                ("f" "u") 0.001233392282229175,
                                ("g" "e") 0.002675372965129461,
                                ("e" "h") 0.001070149186051784,
                                ("l" "l") 0.007944497347299687,
                                ("u" "i") 0.001605223779077677,
                                ("n" "ä") 8.706298462794178E-4,
                                ("u" "t") 0.006076270802158436,
                                ("d" "r") 5.441436539246361E-4,
                                ("h" "t") 0.00140570443930531,
                                ("r" "a") 0.005486781843740081,
                                ("n" "u") 0.00170498344896386,
                                ("h" "d") 8.615607853806738E-4,
                                ("c" "o") 0.006194168593842108,
                                ("o" "f") 0.001142701673241736,
                                ("l" "a") 0.00838888133133814,
                                ("t" "s") 0.002448646442660862,
                                ("p" "a") 0.00417176801342221,
                                ("v" "a") 0.004280596744207137,
                                ("k" "s") 0.0027660635741169,
                                ("w" "n") 5.985580193170997E-4,
                                ("e" "b") 5.350745930258922E-4,
                                ("b" "l") 0.001387566317507822,
                                ("p" "r") 0.004135491769827234,
                                ("t" "u") 0.00506960504239786,
                                ("u" "s") 0.004062939282637283,
                                ("r" "m") 9.431823334693692E-4,
                                ("e" "s") 0.01067428467782161,
                                ("b" "s") 6.348342629120755E-4,
                                ("ä" "k") 5.169364712284043E-4,
                                ("k" "o") 0.003555071872307623,
                                ("y" "e") 7.255248718995148E-4,
                                ("t" "e") 0.01435632340271165,
                                ("l" "s") 0.001224323221330431,
                                ("l" "j") 7.890082981907224E-4,
                                ("s" "k") 0.001378497256609078,
                                ("f" "d") 7.073867501020269E-4,
                                ("w" "f") 8.071464199882102E-4,
                                ("a" "c") 0.002630027660635741,
                                ("s" "s") 0.007037591257425294,
                                ("a" "j") 6.348342629120755E-4,
                                ("p" "p") 0.004081077404434771,
                                ("y" "s") 0.001097356368748016,
                                ("p" "f") 0.002539337051648302,
                                ("k" "e") 0.006656690699678048,
                                ("u" "n") 0.005967442071373509,
                                ("i" "e") 0.00451639232757448,
                                ("k" "n") 4.897292885321725E-4,
                                ("ä" "y") 4.987983494309164E-4,
                                ("d" "e") 0.007382215571577563,
                                ("v" "i") 0.002820477939509364,
                                ("p" "y") 6.529723847095633E-4,
                                ("j" "o") 0.00281140887861062,
                                ("m" "p") 0.003654831542193806,
                                ("s" "n") 0.001487325987394005,
                                ("r" "n") 5.985580193170997E-4,
                                ("v" "e") 0.004652428241055639,
                                ("c" "k") 0.001088287307849272,
                                ("f" "n") 0.001233392282229175,
                                ("k" "ä") 0.001496395048292749,
                                ("f" "l") 0.001260599464925407,
                                ("l" "y") 0.001188046977735455,
                                ("p" "s") 6.711105065070512E-4,
                                ("o" "u") 0.004888223824422981,
                                ("n" "y") 0.001070149186051784,
                                ("p" "u") 0.00225819616378724,
                                ("c" "c") 0.001432911622001542,
                                ("a" "b") 9.88527637963089E-4,
                                ("m" "ä") 0.001088287307849272,
                                ("u" "p") 0.001233392282229175,
                                ("b" "t") 5.260055321271482E-4,
                                ("s" "i") 0.009322994603908764,
                                ("s" "h") 0.001505464109191493,
                                ("r" "u") 0.001487325987394005,
                                ("o" "m") 0.005640955879018728,
                                ("g" "u") 7.799392372919784E-4,
                                ("m" "b") 0.001215254160431687,
                                ("f" "i") 0.003863419942864916,
                                ("u" "d") 7.164558110007709E-4,
                                ("f" "o") 0.001967986215027434,
                                ("ä" "t") 4.897292885321725E-4,
                                ("a" "f") 4.625221058359407E-4,
                                ("m" "u") 0.003364621593434,
                                ("m" "m") 0.002321679590078447,
                                ("a" "p") 0.003527864689611391,
                                ("i" "d") 0.002131229311204825,
                                ("b" "u") 0.001441980682900286,
                                ("l" "ä") 0.001868226545141251,
                                ("b" "b") 0.001668707205368884,
                                ("j" "n") 0.001351290073912846,
                                ("v" "ä") 9.159751507731374E-4,
                                ("k" "u") 0.003301138167142792,
                                ("l" "o") 0.006058132680360949,
                                ("b" "i") 0.002865823244003083,
                                ("o" "a") 0.001333151952115358,
                                ("s" "a") 0.005087743164195348,
                                ("p" "i") 0.003600417176801342,
                                ("j" "u") 0.001886364666938738,
                                ("j" "p") 5.078674103296604E-4,
                                ("o" "o") 0.00230354146828096,
                                ("g" "g") 0.002475853625357094,
                                ("e" "t") 0.008706298462794177,
                                ("c" "l") 0.00251212986895207,
                                ("i" "f") 0.001052011064254296,
                                ("d" "i") 0.003119756949167914,
                                ("n" "a") 0.003591348115902598,
                                ("a" "r") 0.006411826055411962,
                                ("t" "c") 6.257652020133315E-4,
                                ("u" "a") 0.001052011064254296,
                                ("e" "c") 0.002630027660635741,
                                ("i" "m") 0.004443839840384528,
                                ("g" "s") 5.350745930258922E-4,
                                ("c" "d") 5.804198975196118E-4,
                                ("b" "a") 7.708701763932345E-4,
                                ("r" "d") 0.00136035913481159,
                                ("y" "y") 5.713508366208679E-4},
                               :trigram-distribution
                               {("d" "u" "c") 0.001040371185305654,
                                ("n" "k" "i") 8.968717114703913E-4,
                                ("i" "z" "e") 8.609968430115756E-4,
                                ("w" "i" "t") 0.001064287764278198,
                                ("n" "i" "s") 7.533722376351287E-4,
                                ("n" "t" "e") 0.001662202238591792,
                                ("p" "h" "o") 7.772888166076724E-4,
                                ("t" "e" "e") 9.327465799292069E-4,
                                ("e" "r" "e") 0.001100162632737013,
                                ("p" "l" "i") 0.001004496316846838,
                                ("t" "t" "u") 7.055390796900411E-4,
                                ("i" "n" "s") 9.088300009566633E-4,
                                ("a" "i" "s") 8.2512197455276E-4,
                                ("a" "i" "t") 7.294556586625849E-4,
                                ("a" "s" "t") 0.001614369080646704,
                                ("o" "j" "u") 8.2512197455276E-4,
                                ("i" "l" "t") 8.968717114703913E-4,
                                ("a" "a" "t") 7.414139481488567E-4,
                                ("a" "r" "a") 8.849134219841194E-4,
                                ("o" "r" "m") 6.935807902037693E-4,
                                ("r" "e" "m") 8.849134219841194E-4,
                                ("u" "m" "e") 7.17497369176313E-4,
                                ("p" "a" "r") 0.001686118817564336,
                                ("o" "r" "e") 9.088300009566633E-4,
                                ("s" "t" "e") 0.001171912369654645,
                                ("i" "t" "e") 0.001566535922701617,
                                ("u" "s" "e") 0.001219745527599732,
                                ("o" "w" "f") 0.001064287764278198,
                                ("o" "l" "l") 0.001482827896297714,
                                ("l" "a" "t") 7.17497369176313E-4,
                                ("n" "y" "t") 0.001124079211709557,
                                ("c" "r" "e") 9.447048694154788E-4,
                                ("l" "a" "m") 8.729551324978475E-4,
                                ("o" "m" "m") 8.131636850664881E-4,
                                ("m" "p" "a") 9.447048694154788E-4,
                                ("t" "i" "o") 0.002810198029273893,
                                ("e" "p" "l") 7.414139481488567E-4,
                                ("ä" "ä" "n") 0.001530661054242801,
                                ("d" "e" "n") 7.772888166076724E-4,
                                ("o" "w" "n") 7.533722376351287E-4,
                                ("k" "e" "l") 0.00101645460633311,
                                ("e" "s" "i") 6.935807902037693E-4,
                                ("s" "i" "a") 6.935807902037693E-4,
                                ("m" "a" "n") 0.00101645460633311,
                                ("s" "e" "l") 8.968717114703913E-4,
                                ("i" "o" "n") 0.00324069645077968,
                                ("o" "r" "i") 7.892471060939443E-4,
                                ("m" "e" "r") 9.088300009566633E-4,
                                ("l" "i" "e") 0.001100162632737013,
                                ("s" "e" "t") 0.001710035396536879,
                                ("v" "i" "e") 0.001124079211709557,
                                ("f" "r" "e") 7.772888166076724E-4,
                                ("a" "v" "a") 0.001159954080168373,
                                ("i" "s" "i") 0.00139911986989381,
                                ("a" "a" "a") 0.00387448579355209,
                                ("l" "e" "v") 7.294556586625849E-4,
                                ("a" "r" "t") 8.729551324978475E-4,
                                ("e" "s" "s") 0.002582990529034727,
                                ("r" "o" "d") 0.001446953027838898,
                                ("t" "i" "s") 7.055390796900411E-4,
                                ("u" "t" "i") 7.533722376351287E-4,
                                ("a" "c" "t") 8.849134219841194E-4,
                                ("v" "e" "r") 0.001721993686023151,
                                ("w" "o" "r") 0.001542619343729073,
                                ("u" "n" "t") 0.001423036448866354,
                                ("e" "n" "k") 7.772888166076724E-4,
                                ("e" "i" "n") 6.816225007174974E-4,
                                ("u" "u" "t") 9.686214483880226E-4,
                                ("o" "d" "o") 7.892471060939443E-4,
                                ("o" "n" "e") 7.892471060939443E-4,
                                ("n" "g" "e") 7.17497369176313E-4,
                                ("o" "n" "s") 8.012053955802162E-4,
                                ("u" "n" "n") 0.001040371185305654,
                                ("j" "o" "s") 8.609968430115756E-4,
                                ("i" "v" "e") 9.566631589017507E-4,
                                ("m" "a" "t") 0.001004496316846838,
                                ("i" "p" "l") 8.490385535253037E-4,
                                ("k" "i" "n") 0.001434994738352626,
                                ("t" "e" "t") 7.533722376351287E-4,
                                ("i" "k" "k") 7.653305271214006E-4,
                                ("t" "t" "i") 0.00126757868554482,
                                ("f" "o" "r") 0.001913326317803501,
                                ("u" "s" "t") 0.00145891131732517,
                                ("o" "u" "t") 0.001853534870372142,
                                ("w" "f" "l") 8.609968430115756E-4,
                                ("i" "s" "t") 0.003252654740265952,
                                ("u" "c" "t") 7.294556586625849E-4,
                                ("h" "a" "n") 0.001518702764756529,
                                ("e" "a" "d") 0.001351286711948723,
                                ("u" "n" "k") 7.653305271214006E-4,
                                ("c" "t" "i") 0.001052329474791926,
                                ("e" "l" "e") 0.00107624605376447,
                                ("t" "ä" "ä") 0.001566535922701617,
                                ("l" "a" "k") 7.653305271214006E-4,
                                ("s" "s" "o") 7.892471060939443E-4,
                                ("i" "g" "q") 0.001698077107050607,
                                ("a" "i" "k") 9.805797378742945E-4,
                                ("p" "p" "p") 0.001674160528078064,
                                ("p" "r" "o") 0.003707069740744284,
                                ("t" "i" "e") 0.001470869606811442,
                                ("i" "i" "t") 8.968717114703913E-4,
                                ("a" "s" "s") 0.001662202238591792,
                                ("k" "u" "n") 0.00139911986989381,
                                ("a" "p" "i") 0.001052329474791926,
                                ("j" "u" "k") 7.892471060939443E-4,
                                ("m" "e" "n") 0.002427532765713192,
                                ("v" "o" "i") 9.20788290442935E-4,
                                ("u" "s" "i") 8.609968430115756E-4,
                                ("y" "o" "u") 0.00145891131732517,
                                ("o" "p" "t") 8.849134219841194E-4,
                                ("n" "c" "e") 9.805797378742945E-4,
                                ("m" "o" "n") 0.002104658949583852,
                                ("t" "u" "s") 6.816225007174974E-4,
                                ("s" "s" "ä") 9.925380273605663E-4,
                                ("t" "r" "e") 7.653305271214006E-4,
                                ("p" "e" "r") 0.001446953027838898,
                                ("p" "r" "e") 9.088300009566633E-4,
                                ("ä" "i" "s") 6.696642112312255E-4,
                                ("l" "l" "ä") 0.001638285659619248,
                                ("p" "t" "i") 9.20788290442935E-4,
                                ("c" "o" "m") 0.002798239739787621,
                                ("a" "t" "i") 0.00324069645077968,
                                ("a" "t" "e") 0.002511240792117096,
                                ("e" "n" "d") 8.370802640390319E-4,
                                ("a" "t" "t") 0.00107624605376447,
                                ("s" "o" "n") 8.729551324978475E-4,
                                ("a" "i" "n") 0.00184157658088587,
                                ("e" "n" "t") 0.003121113555916962,
                                ("i" "t" "ä") 0.001937242896776045,
                                ("m" "u" "t") 8.370802640390319E-4,
                                ("i" "e" "r") 8.012053955802162E-4,
                                ("m" "m" "a") 9.566631589017507E-4,
                                ("f" "i" "l") 0.001626327370132976,
                                ("m" "a" "p") 0.00139911986989381,
                                ("s" "t" "o") 0.001231703817086004,
                                ("i" "i" "n") 0.002164450397015211,
                                ("a" "p" "p") 8.2512197455276E-4,
                                ("i" "n" "p") 6.696642112312255E-4,
                                ("a" "r" "e") 8.490385535253037E-4,
                                ("n" "o" "w") 0.002260116712905386,
                                ("o" "d" "e") 8.370802640390319E-4,
                                ("e" "l" "i") 0.002068784081125036,
                                ("i" "e" "n") 6.935807902037693E-4,
                                ("l" "l" "a") 0.002810198029273893,
                                ("f" "i" "n") 9.20788290442935E-4,
                                ("o" "j" "e") 9.088300009566633E-4,
                                ("u" "t" "t") 0.001470869606811442,
                                ("m" "a" "a") 7.653305271214006E-4,
                                ("a" "m" "e") 0.001686118817564336,
                                ("o" "l" "e") 9.925380273605663E-4,
                                ("t" "u" "n") 0.001100162632737013,
                                ("e" "c" "t") 0.001243662106572276,
                                ("s" "i" "m") 6.577059217449535E-4,
                                ("a" "d" "d") 0.00107624605376447,
                                ("l" "a" "y") 0.00145891131732517,
                                ("m" "p" "l") 7.892471060939443E-4,
                                ("j" "u" "r") 8.370802640390319E-4,
                                ("i" "t" "h") 0.001004496316846838,
                                ("i" "g" "h") 7.653305271214006E-4,
                                ("v" "a" "i") 9.805797378742945E-4,
                                ("k" "s" "i") 0.001375203290921267,
                                ("e" "t" "e") 7.892471060939443E-4,
                                ("e" "t" "t") 0.003336362766669856,
                                ("a" "m" "p") 0.001064287764278198,
                                ("t" "t" "a") 0.002582990529034727,
                                ("o" "n" "o") 0.001088204343250741,
                                ("t" "o" "r") 0.001387161580407539,
                                ("h" "a" "t") 0.001255620396058548,
                                ("b" "i" "g") 0.002965655792595427,
                                ("t" "e" "n") 0.001817660001913326,
                                ("n" "u" "m") 8.2512197455276E-4,
                                ("a" "n" "t") 0.001064287764278198,
                                ("b" "r" "e") 6.577059217449535E-4,
                                ("b" "d" "e") 6.935807902037693E-4,
                                ("r" "e" "s") 0.002367741318281833,
                                ("i" "e" "l") 0.00120778723811346,
                                ("m" "b" "d") 7.055390796900411E-4,
                                ("e" "n" "e") 7.653305271214006E-4,
                                ("a" "v" "e") 0.001124079211709557,
                                ("m" "a" "r") 7.055390796900411E-4,
                                ("s" "e" "n") 0.001710035396536879,
                                ("n" "e" "s") 8.131636850664881E-4,
                                ("h" "e" "n") 9.447048694154788E-4,
                                ("l" "i" "s") 0.002008992633693676,
                                ("i" "n" "k") 6.935807902037693E-4,
                                ("n" "n" "e") 8.370802640390319E-4,
                                ("o" "t" "t") 7.294556586625849E-4,
                                ("r" "s" "t") 8.2512197455276E-4,
                                ("f" "r" "o") 0.001112120922223285,
                                ("t" "h" "e") 0.004986606715775375,
                                ("k" "k" "a") 0.001327370132976179,
                                ("i" "n" "g") 0.004233234478140247,
                                ("e" "s" "t") 0.00406581842533244,
                                ("i" "l" "l") 0.002690615134411174,
                                ("e" "a" "t") 8.012053955802162E-4,
                                ("i" "t" "i") 6.696642112312255E-4,
                                ("o" "t" "a") 8.609968430115756E-4,
                                ("r" "o" "j") 0.00107624605376447,
                                ("t" "e" "i") 6.696642112312255E-4,
                                ("s" "c" "c") 9.805797378742945E-4,
                                ("o" "i" "n") 0.001315411843489907,
                                ("r" "a" "t") 0.002044867502152492,
                                ("i" "e" "w") 7.533722376351287E-4,
                                ("a" "g" "e") 0.001339328422462451,
                                ("t" "a" "i") 0.001446953027838898,
                                ("t" "u" "o") 0.001147995790682101,
                                ("t" "e" "m") 7.294556586625849E-4,
                                ("e" "r" "i") 8.490385535253037E-4,
                                ("l" "e" "a") 8.968717114703913E-4,
                                ("u" "l" "t") 0.001757868554481967,
                                ("i" "t" "t") 0.002068784081125036,
                                ("a" "b" "l") 9.925380273605663E-4,
                                ("r" "o" "w") 0.001446953027838898,
                                ("n" "n" "n") 0.001626327370132976,
                                ("o" "i" "s") 9.447048694154788E-4,
                                ("t" "i" "m") 0.002379699607768105,
                                ("g" "q" "u") 0.001506744475270257,
                                ("m" "i" "t") 8.849134219841194E-4,
                                ("e" "r" "t") 9.925380273605663E-4,
                                ("i" "d" "e") 6.816225007174974E-4,
                                ("o" "u" "l") 7.892471060939443E-4,
                                ("b" "d" "a") 6.935807902037693E-4,
                                ("g" "g" "g") 0.002451449344685736,
                                ("o" "s" "t") 0.001626327370132976,
                                ("r" "o" "m") 0.00120778723811346,
                                ("e" "t" "a") 0.001004496316846838,
                                ("l" "t" "e") 9.686214483880226E-4,
                                ("i" "m" "e") 0.002379699607768105,
                                ("s" "s" "i") 0.00120778723811346,
                                ("n" "o" "t") 0.001375203290921267,
                                ("o" "d" "u") 0.00101645460633311,
                                ("n" "i" "i") 0.001434994738352626,
                                ("v" "e" "l") 0.001052329474791926,
                                ("m" "e" "s") 0.001231703817086004,
                                ("o" "u" "n") 0.001578494212187889,
                                ("e" "r" "s") 0.001949201186262317,
                                ("s" "i" "i") 8.370802640390319E-4,
                                ("c" "o" "n") 0.00190136802831723,
                                ("f" "f" "f") 8.370802640390319E-4,
                                ("l" "a" "s") 0.001363245001434995,
                                ("c" "o" "r") 6.696642112312255E-4,
                                ("t" "a" "n") 8.370802640390319E-4,
                                ("i" "n" "t") 0.002260116712905386,
                                ("a" "a" "n") 0.003324404477183584,
                                ("s" "i" "n") 0.002487324213144552,
                                ("s" "t" "i") 0.002355783028795561,
                                ("l" "o" "j") 8.849134219841194E-4,
                                ("e" "a" "s") 8.131636850664881E-4,
                                ("o" "r" "d") 0.001339328422462451,
                                ("a" "t" "a") 0.001985076054721133,
                                ("l" "l" "i") 7.294556586625849E-4,
                                ("t" "t" "e") 0.002272075002391658,
                                ("i" "s" "s") 0.001423036448866354,
                                ("t" "i" "c") 7.892471060939443E-4,
                                ("t" "t" "ä") 0.002343824739309289,
                                ("o" "k" "k") 9.686214483880226E-4,
                                ("n" "d" "e") 7.17497369176313E-4,
                                ("s" "s" "s") 9.20788290442935E-4,
                                ("r" "m" "a") 8.131636850664881E-4,
                                ("n" "n" "i") 0.00107624605376447,
                                ("s" "i" "o") 6.696642112312255E-4,
                                ("f" "i" "r") 8.729551324978475E-4,
                                ("l" "o" "g") 0.001769826843968239,
                                ("p" "l" "e") 7.772888166076724E-4,
                                ("k" "k" "e") 8.849134219841194E-4,
                                ("t" "a" "t") 0.001721993686023151,
                                ("o" "n" "t") 9.925380273605663E-4,
                                ("m" "u" "u") 6.935807902037693E-4,
                                ("o" "m" "e") 9.686214483880226E-4,
                                ("i" "m" "i") 0.00145891131732517,
                                ("m" "a" "s") 8.609968430115756E-4,
                                ("i" "s" "e") 0.001327370132976179,
                                ("e" "m" "o") 0.001303453554003635,
                                ("t" "e" "s") 0.002343824739309289,
                                ("e" "l" "l") 0.001411078159380082,
                                ("k" "e" "y") 0.002104658949583852,
                                ("a" "y" "o") 0.001100162632737013,
                                ("t" "o" "i") 0.001530661054242801,
                                ("s" "s" "a") 0.002618865397493543,
                                ("c" "a" "l") 7.294556586625849E-4,
                                ("e" "m" "p") 9.566631589017507E-4,
                                ("t" "o" "t") 8.012053955802162E-4,
                                ("m" "i" "n") 0.001124079211709557,
                                ("a" "t" "o") 0.001554577633215345,
                                ("y" "e" "s") 6.935807902037693E-4,
                                ("m" "o" "i") 7.533722376351287E-4,
                                ("u" "o" "t") 6.696642112312255E-4,
                                ("r" "i" "n") 0.001279536975031092,
                                ("k" "a" "a") 0.001136037501195829,
                                ("c" "o" "u") 0.001159954080168373,
                                ("u" "l" "l") 7.294556586625849E-4,
                                ("l" "t" "i") 9.566631589017507E-4,
                                ("m" "u" "l") 0.001327370132976179,
                                ("a" "r" "i") 0.001004496316846838,
                                ("s" "t" "r") 0.002212283554960298,
                                ("s" "u" "m") 7.055390796900411E-4,
                                ("r" "a" "c") 8.490385535253037E-4,
                                ("s" "u" "l") 9.327465799292069E-4,
                                ("q" "u" "e") 0.003276571319238496,
                                ("b" "l" "e") 0.00120778723811346,
                                ("r" "i" "t") 7.892471060939443E-4,
                                ("n" "t" "a") 0.001088204343250741,
                                ("s" "t" "a") 0.005381230268822348,
                                ("d" "a" "t") 0.001638285659619248,
                                ("t" "a" "r") 0.001279536975031092,
                                ("c" "h" "a") 9.447048694154788E-4,
                                ("d" "e" "v") 0.001542619343729073,
                                ("a" "n" "s") 6.696642112312255E-4,
                                ("t" "e" "d") 8.849134219841194E-4,
                                ("e" "s" "u") 7.892471060939443E-4,
                                ("i" "r" "s") 8.490385535253037E-4,
                                ("o" "i" "t") 0.001100162632737013,
                                ("t" "e" "r") 0.002881947766191524,
                                ("r" "a" "n") 0.001112120922223285,
                                ("d" "i" "s") 9.20788290442935E-4,
                                ("s" "i" "t") 0.001494786185783985,
                                ("l" "i" "n") 0.001470869606811442,
                                ("n" "a" "m") 0.001375203290921267,
                                ("a" "n" "g") 7.414139481488567E-4,
                                ("a" "s" "e") 0.00101645460633311,
                                ("e" "e" "t") 7.892471060939443E-4,
                                ("t" "r" "a") 8.609968430115756E-4,
                                ("o" "t" "e") 6.816225007174974E-4,
                                ("t" "r" "i") 0.001004496316846838,
                                ("t" "a" "a") 0.002750406581842533,
                                ("i" "n" "e") 0.00209270066009758,
                                ("v" "a" "l") 0.001171912369654645,
                                ("e" "v" "e") 0.001411078159380082,
                                ("t" "i" "p") 9.20788290442935E-4,
                                ("k" "s" "e") 9.686214483880226E-4,
                                ("s" "e" "s") 0.001100162632737013,
                                ("h" "h" "h") 0.001231703817086004,
                                ("l" "e" "s") 8.609968430115756E-4,
                                ("c" "l" "o") 0.001255620396058548,
                                ("b" "c" "o") 9.088300009566633E-4,
                                ("e" "q" "u") 7.533722376351287E-4,
                                ("i" "k" "a") 9.20788290442935E-4,
                                ("t" "h" "a") 9.088300009566633E-4,
                                ("u" "k" "k") 9.447048694154788E-4,
                                ("e" "r" "y") 0.002630823686979814,
                                ("p" "i" "t") 9.925380273605663E-4,
                                ("a" "n" "d") 0.002810198029273893,
                                ("o" "l" "i") 0.00107624605376447,
                                ("m" "i" "s") 0.001088204343250741,
                                ("r" "e" "e") 7.294556586625849E-4,
                                ("i" "t" "a") 0.001291495264517363,
                                ("u" "r" "e") 0.001112120922223285,
                                ("t" "o" "m") 0.001470869606811442,
                                ("c" "h" "e") 7.653305271214006E-4,
                                ("l" "l" "e") 0.001566535922701617,
                                ("t" "a" "k") 0.00101645460633311,
                                ("o" "n" "i") 7.772888166076724E-4,
                                ("r" "e" "c") 6.935807902037693E-4,
                                ("e" "x" "t") 0.001530661054242801,
                                ("a" "k" "e") 0.002020950923179948,
                                ("r" "e" "a") 0.002236200133932842,
                                ("n" "o" "k") 8.849134219841194E-4,
                                ("s" "t" "ä") 0.001147995790682101,
                                ("i" "t" "o") 8.609968430115756E-4,
                                ("l" "e" "t") 0.001040371185305654,
                                ("i" "n" "c") 6.696642112312255E-4,
                                ("a" "m" "b") 7.653305271214006E-4,
                                ("s" "a" "m") 7.055390796900411E-4,
                                ("s" "o" "u") 7.892471060939443E-4,
                                ("h" "e" "r") 8.609968430115756E-4,
                                ("u" "e" "r") 0.002630823686979814,
                                ("k" "a" "n") 9.805797378742945E-4,
                                ("t" "e" "x") 0.001100162632737013,
                                ("t" "o" "d") 8.609968430115756E-4,
                                ("e" "r" "a") 8.609968430115756E-4,
                                ("s" "n" "o") 0.001590452501674161,
                                ("n" "e" "n") 0.001327370132976179,
                                ("e" "e" "n") 0.001506744475270257,
                                ("e" "f" "n") 7.294556586625849E-4,
                                ("r" "e" "q") 7.653305271214006E-4,
                                ("s" "a" "a") 8.131636850664881E-4,
                                ("a" "l" "l") 0.002008992633693676,
                                ("t" "u" "l") 8.849134219841194E-4,
                                ("u" "k" "s") 7.055390796900411E-4,
                                ("d" "i" "n") 6.696642112312255E-4,
                                ("o" "r" "t") 9.327465799292069E-4,
                                ("r" "e" "p") 8.968717114703913E-4,
                                ("t" "i" "n") 0.001913326317803501,
                                ("d" "e" "f") 0.00203290921266622,
                                ("t" "i" "l") 8.370802640390319E-4,
                                ("n" "s" "i") 8.012053955802162E-4,
                                ("o" "v" "e") 9.686214483880226E-4,
                                ("i" "l" "e") 0.001470869606811442,
                                ("t" "e" "l") 8.012053955802162E-4,
                                ("o" "m" "p") 0.001805701712427054,
                                ("l" "a" "n") 6.577059217449535E-4,
                                ("f" "l" "a") 9.925380273605663E-4},
                               :character-distribution
                               {"d" 0.02807035484200839,
                                "n" 0.07286021065548624,
                                "z" 0.002008677486742729,
                                "w" 0.009743911881144728,
                                "s" 0.06739660789154601,
                                "f" 0.01861130994989263,
                                "e" 0.09610973952931208,
                                "q" 0.003286926796488101,
                                "p" 0.03275239945656144,
                                "j" 0.01390004820825968,
                                "x" 0.003915095028705827,
                                "v" 0.01296510014170307,
                                "a" 0.09051465969351234,
                                "t" 0.08933867032854659,
                                "i" 0.08189560720494354,
                                "k" 0.02985259959388193,
                                "b" 0.01514908039092516,
                                "r" 0.04584897667012403,
                                "y" 0.01605481133040188,
                                "g" 0.01734036492191723,
                                "l" 0.04820825968182549,
                                "u" 0.03777044103253327,
                                "ä" 0.01681445663447913,
                                "h" 0.02116780856938337,
                                "ö" 0.00400274640994551,
                                "m" 0.03373847749550787,
                                "o" 0.06719939228375674,
                                "c" 0.02348326589046499}})
