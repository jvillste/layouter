(ns layouter.optimization-progress-view
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [flow-gl.gui.path :as path]
   [fungl.color :as color]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [layouter.gui :as gui]
   [layouter.optimize :as optimize]
   [layouter.rating :as rating]
   [layouter.text :as text]
   [layouter.view :as view]
   [medley.core :as medley]))



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
  (if (empty? points)
    points
    (map (partial scale-point {:x (double (/ width
                                             (max 1e-9
                                                  (- (apply max (map :x points))
                                                     (apply min (map :x points))))))
                               :y (double (/ height
                                             (max 1e-9
                                                  (- (apply max (map :y points))
                                                     (apply min (map :y points))))))})
         points)))

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
  (if (empty? points)
    points
    (map (partial add-points
                  {:x (- (apply min (map :x points)))
                   :y (- (apply min (map :y points)))})
         points)))

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
  (gui/text (str key " " (key @optimize/optimization-state-atom))))


(defn enrich-state [state]
  (-> state
      (cond-> (:ratings state)
        (-> (assoc :best-rating (optimize/best-rating (:ratings state)))
            (dissoc :ratings)))
      (merge (optimize/next-generation-parameters (- (:generation-number state)
                                                     (:last-improved-generation-number state))
                                                  (:metaparameters state)))
      (assoc :generations-since-last-improvement
             (- (:generation-number state)
                (:last-improved-generation-number state)))))

(defn optimization-progress-view [history-atom]
  (layouts/with-margin 10
    (if (or (empty? @history-atom)
            (empty? (:ratings (last @history-atom))))
      (gui/text "no history")
      (let [enriched-states (->> @history-atom
                                 (map enrich-state))

            displayed-keys [:generation-number
                            :best-rating
                            :rating-diversity
                            :layout-diversity
                            :layout-entropy

                            ;; :elite-proportion
                            ;; :parent-selection-temperature
                            ;; :mutation-propability
                            :random-solution-proportion
                            :generations-since-last-improvement]
            key-to-color (into {} (for [[index key] (map vector (range) displayed-keys)]
                                    [key (concat (color/hsl-to-rgb (* 360
                                                                      (/ index
                                                                         (count displayed-keys)))
                                                                   0.5
                                                                   0.5)
                                                 [1.0])]))
            graph-height 300]

        (apply layouts/vertically-2
               {:margin 10}
               (apply layouts/superimpose
                      (for [key (->> displayed-keys
                                     (remove #{:generation-number
                                               :generations-since-last-improvement}))]
                        (let [states (->> enriched-states
                                          (remove (comp nil? key)))]
                          (when (not (empty? states))
                            (layouts/with-margin 50
                              (path/path (key-to-color key)
                                         5
                                         (->> states
                                              (map (fn [state]
                                                     {:x (:generation-number state)
                                                      :y (key state)}))
                                              (scale-to-view graph-height graph-height)
                                              (map (partial scale-point {:x 1 :y -1}))
                                              (move-to-origin))))))))

               (for [key displayed-keys]
                 (gui/text (let [values (remove nil? (map key enriched-states))]
                             (str key " " (key (last enriched-states))
                                  (let [])
                                  (when (not (empty? values))
                                    (str " min: " (apply min values)))
                                  (when (not (empty? values))
                                    (str " max: " (apply max values)))))
                           {:color (key-to-color key)}))

               (let [metaparameters (:metaparameters (last enriched-states))]
                 (for [key (sort-by name (keys metaparameters))]
                   (gui/text (str key ": " (key metaparameters))))))))))


(defn metaoptimization-progress-view []
  (gui/black-background
   (layouts/vertically-2 {:margin 10}
                         [optimization-progress-view optimize/metaoptimization-history-atom]
                         [optimization-progress-view optimize/optimization-history-atom])))

(def optimized-metaparameters {:minimum-random-solution-proportion 0.0715794243512767,
                               :mutation-propability-slope 0.030556485134785514,
                               :minimum-parent-selection-temperature 0.9262451289094256,
                               :elite-proportion-slope 0.01891647926531371,
                               :parent-selection-temperature-slope 0.0303881869706002,
                               :maximum-mutation-propability 0.3007288229700735,
                               :maximum-random-solution-proportion 0.33671162677712707,
                               :minimum-elite-proportion 0.03592704160978212,
                               :random-solution-proportion-slope 0.016129677521803748,
                               :maximum-parent-selection-temperature 7.429768803279241,
                               :maximum-elite-proportion 0.1478989032110839,
                               :minimum-mutation-propability 0.03374161054458928,
                               :population-size 500})

(defn tail [maximum-lines file-name]
  (with-open [raf (java.io.RandomAccessFile. file-name "r")]
    (loop [lines-read 0
           lines []
           line []
           position (dec (.length raf))]
      (.seek raf position)
      (let [byte (.read raf)]
        (cond (= 0 position)
              (reverse (conj lines (apply str (reverse (conj line (char byte))))))

              (and (= \newline (char byte))
                   (= position (dec (.length raf))))
              (recur lines-read
                     lines
                     line
                     (dec position))

              (and (= \newline (char byte))
                   (= maximum-lines (inc (count lines))))
              (reverse (conj lines (apply str (reverse line))))

              (= \newline (char byte))
              (recur (inc lines-read)
                     (conj lines (apply str (reverse line)))
                     []
                     (dec position))

              :else
              (recur lines-read
                     lines
                     (conj line (char byte))
                     (dec position)))))))

(defn read-log [log-file-path]
  (if (not (.exists (io/file log-file-path)))
    []
    (->> (io/reader log-file-path)
         (line-seq)
         (map edn/read-string))))

(defn rated-metaparameters [log-file-path]
  (if (not (.exists (io/file log-file-path)))
    []
    (->> #_(tail 0 log-file-path)
         (read-log log-file-path)
         (map (fn [state]
                [(:metaparameters state)
                 (optimize/best-rating (:ratings state))]))
         (sort-by second))))

(defn optimize-metaparameters [log-file-path]
  (optimize/optimize optimize/random-metaparameters
                     optimize/crossbreed-metaparameters
                     (fn [metaparameter-set]
                       (map (fn [metaparameters]
                              (let [state (optimize/optimize optimize/random-layout
                                                             optimize/crossbreed-layouts
                                                             (partial optimize/layouts-to-ratings text/hybrid-statistics)
                                                             optimize/mutate-layout
                                                             {:metaparameters (assoc metaparameters
                                                                                     :population-size 200)
                                                              :number-of-generations 500
                                                              :logging-frequency 100})]
                                (spit log-file-path
                                      (str (pr-str (update state
                                                           :ratings
                                                           (fn [ratings]
                                                             (->> ratings
                                                                  (sort-by second)
                                                                  (take 10)))))
                                           "\n")
                                      :append true)
                                [metaparameters (optimize/best-rating (:ratings state))]))
                            metaparameter-set))
                     optimize/mutate-metaparameters
                     (let [population-size 20]
                       {:metaparameters {:population-size 20}
                        ;; :number-of-generations 10
                        :initial-ratings (->> (rated-metaparameters log-file-path)
                                              (take population-size))
                        #_(->> @optimize/metaoptimization-history-atom
                               (last)
                               (:ratings))
                        :history-atom optimize/metaoptimization-history-atom})))

(defonce layout-optimization-log-atom (dependable-atom/atom []))

(defn optimize-layout [metaparameters multipliers text-statistics log-file-path & [options]]
  (let [state (-> (optimize/optimize optimize/random-layout
                                     optimize/crossbreed-layouts
                                     (fn [layouts]
                                       (binding [rating/multipliers multipliers]
                                         (optimize/layouts-to-ratings text-statistics
                                                                      layouts)))
                                     optimize/mutate-layout
                                     (merge {:metaparameters metaparameters
                                             #_(random-metaparameters)
                                             ;; :number-of-generations 30
                                             ;; :initial-ratings (->> @optimization-state-atom
                                             ;;                       (last))
                                             :logging-frequency 100
                                             :history-atom optimize/optimization-history-atom}
                                            options))
                  (assoc :text-statistics-name (:name text-statistics)
                         :multipliers multipliers)
                  (update :ratings (fn [ratings]
                                     (->> ratings
                                          (sort-by second)
                                          (take 10)))))]
    (swap! layout-optimization-log-atom conj state)
    (spit log-file-path
          (str (pr-str state)
               "\n")
          :append true)
    state))

(def metaparameter-optimization-log-file-path "temp/metaparameter-optimization-log.edn")
(def layout-optimization-log-file-path "temp/layout-optimization-log.edn")

(def optimized-layouts-atom (atom []))

(def static-metaparameters {:population-size 200

                            :elite-proportion-slope 0.0
                            :minimum-elite-proportion 0.3
                            :maximum-elite-proportion 0.3

                            :minimum-parent-selection-temperature 1.0
                            :maximum-parent-selection-temperature 1.0
                            :parent-selection-temperature-slope 0.0

                            :mutation-propability-slope 0.0
                            :minimum-mutation-propability 0.2
                            :maximum-mutation-propability 0.2

                            :random-solution-proportion-slope 0.0
                            :minimum-random-solution-proportion 0.0
                            :maximum-random-solution-proportion 0.0})

(defn optimize-repeatedly! [optimize-new-layout!]
  (loop [round 0]
    (println "repeated optimization round" round)
    (optimize-new-layout!)
    (if @optimize/stop-requested?-atom
      (println "stopped repeated optimization")
      (recur (inc round)))))

(defn best-layouts-per-statistics-and-multipliers [log]
  (->> log
       ;; (take 1)
       (group-by (juxt :text-statistics-name :multipliers))
       (medley/map-vals (fn [states]
                          (->> states
                               (mapcat :ratings)
                               (sort-by second)
                               (map first)
                               (take 1)
                               (map (fn [layout]
                                      (assoc (select-keys (first states)
                                                          [:text-statistics-name :multipliers])
                                             :layout layout))))))
       (vals)
       (apply concat)

       ;; (count)
       ))

(comment
  (last @optimize/optimization-history-atom)

  (do (reset! optimize/optimization-history-atom [])
      (reset! optimize/metaoptimization-history-atom []))

  (reset! view/event-channel-atom nil)

  (view/start-view #'metaoptimization-progress-view)

  (view/start-view (fn [] (gui/black-background [optimization-progress-view optimize/optimization-history-atom]))
                   ;;{:join? true}
                   )

  ;; hot-right-now TODO: remove me

  (do (reset! optimize/stop-requested?-atom false)
      (optimize-metaparameters metaparameter-optimization-log-file-path))

  (do (reset! optimize/stop-requested?-atom false)
      #_(optimize-layout (first (first (rated-metaparameters metaparameter-optimization-log-file-path))))
      )

  (count (rated-metaparameters metaparameter-optimization-log-file-path))
  (first (rated-metaparameters metaparameter-optimization-log-file-path))


  (doto (Thread. (fn []
                   (reset! optimize/stop-requested?-atom false)
                   (optimize-metaparameters metaparameter-optimization-log-file-path)))
    (.setName "metaparameter optimization")
    (.start))

  (doto (Thread. (fn []
                   (println "started")
                   (reset! optimize/stop-requested?-atom false)

                   (optimize-layout static-metaparameters
                                    {:maximum-number-of-generations-without-improvement 200})
                   ))
    (.setName "layout optimization")
    (.start))


  (doto (Thread. (fn []
                   (reset! optimize/stop-requested?-atom false)
                   (let [text-statistics #_text/finnish-statistics #_text/english-statistics text/hybrid-statistics]
                     (optimize-repeatedly! #_#(optimize/hill-climb-all text/hybrid-statistics
                                                                       (optimize/random-layout))
                                           (fn []
                                             (optimize-layout static-metaparameters
                                                              {:digram-roll 1
                                                               :trigram-roll 0
                                                               :key-rating 1
                                                               :finger-type 0.1
                                                               :horizontal-movement 0.1
                                                               :vertical-movement 1
                                                               :hand-balance 0.1}
                                                              text-statistics
                                                              layout-optimization-log-file-path
                                                              {:maximum-number-of-generations-without-improvement 300}))))))
    (.setName "repeating layout optimization")
    (.start))

  (reset! optimize/stop-requested?-atom true)

  ;; hot-right-now TODO: remove me

  (do (reset! layout-optimization-log-atom (read-log layout-optimization-log-file-path))
      nil)
  (best-layouts-per-statistics-and-multipliers (read-log layout-optimization-log-file-path))
  (best-layouts-per-statistics-and-multipliers @layout-optimization-log-atom)

  (count @optimized-layouts-atom)
  (reset! optimized-layouts-atom [])

  (optimize-layout (assoc static-metaparameters
                          :population-size 200

                          :elite-proportion-slope 0.0
                          :minimum-elite-proportion 0.3
                          :maximum-elite-proportion 0.3

                          :minimum-parent-selection-temperature 5.0
                          :maximum-parent-selection-temperature 1.0
                          :parent-selection-temperature-slope -0.05

                          :mutation-propability-slope 0.0
                          :minimum-mutation-propability 0.2
                          :maximum-mutation-propability 0.2

                          :random-solution-proportion-slope 0.0
                          :minimum-random-solution-proportion 0.0
                          :maximum-random-solution-proportion 0.0)

                   { ;; :maximum-number-of-generations-without-improvement 1000
                    :initial-ratings (optimize/layouts-to-ratings text/hybrid-statistics
                                                                  @optimized-layouts-atom)})

  (def hill-climbing-optimized-layouts )

  (reset! optimize/stop-requested?-atom true)

  (->> (tail 10 metaparameter-optimization-log-file-path)
       (map edn/read-string)
       (map (fn [state]
              (optimize/best-rating (:ratings state)))))

  (edn/read-string (first (tail 1 metaparameter-optimization-log-file-path)))

  (let [reader (java.io.PushbackReader. (java.io.StringReader. "{:a 1}{:b 2}"))]
    [(read reader)
     (read reader)
     (read {:eof nil}
           reader)])

  (->> @optimize/metaoptimization-history-atom
       (last)
       :ratings
       (sort-by second)
       first
       first)

  (spit "temp/last-metaoptimzation-state2.edn"
        (pr-str (last @optimize/metaoptimization-history-atom)))

  (spit "temp/last-optimzation-state-4.edn"
        (pr-str (last @optimize/optimization-history-atom)))

  (spit "temp/optimized-layouts.edn"
        (pr-str (last @optimized-layouts-atom)))

  (->> @optimize/optimization-history-atom
       (last)
       :ratings
       (sort-by second)
       #_first
       #_(map first)
       (map second)
       (distinct)
       #_(count)
       #_(take 5))

  (->> @optimize/optimization-history-atom
       (last)
       :ratings
       (sort-by second)
       (map first)
       (distinct)
       #_(count)
       (take 5))




  ;; 1.0585756223599754 breeding with 0-2 mutations
  ;; 1.0586912307882292 only mutations, no breeding
  ;; 1.0451653252876312
  ;; {:generation-number 2020, :last-improved-generation-number 1478, :best-rating 1.0234699909057077, :population-size 500, :elite-proportion 0.05, :parent-selection-temperature 10.0, :mutation-propability 0.2, :random-solution-proportion 0.5}

  (let [layout #_(optimize text-statistics)
        (hill-climb-all text-statistics
                        #_(random-layout (keys (:character-distribution text-statistics)))
                        (optimize text-statistics))]
    (println (rate-layout text-statistics
                          layout))
    layout)

  (repeatedly 2
              (fn []
                (let [text-statistics hybrid-statistics
                      ga-optimized-layout (optimize text-statistics)
                      gd-optimized-layout (hill-climb-all text-statistics
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
        gd-optimized-layout (hill-climb-all text-statistics
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



(defn raf-seq
  [#^java.io.RandomAccessFile raf]
  (if-let [line (.readLine raf)]
    (lazy-seq (cons line (raf-seq raf)))
    []))

(defn tail-seq [input]
  (let [raf (java.io.RandomAccessFile. input "r")]
    (.seek raf (.length raf))
    (raf-seq raf)))

(comment
  (spit "temp/log-test.edn" "{:a 1}\n{:b 2}")
  (slurp "temp/log-test.edn")
  (tail-seq "temp/log-test.edn")
  (read-last-n-lines "temp/log-test.edn" 1)
  ) ;; TODO: remove me



(comment
  (-> (->> @optimize/optimization-history-atom
           (last))
      (update :ratings (partial take 5)))
  )


(def best-optimization-run '{:metaparameters
                             {:mutation-propability-slope 0.0,
                              :minimum-parent-selection-temperature 1.0,
                              :elite-proportion-slope 0.0,
                              :parent-selection-temperature-slope 0.0,
                              :maximum-mutation-propability 0.2,
                              :random-solution-proportion-slope 0.0,
                              :minimum-elite-proportion 0.3,
                              :maximum-parent-selection-temperature 1.0,
                              :minimum-random-solution-proportion 0.0,
                              :maximum-elite-proportion 0.3,
                              :minimum-mutation-propability 0.2,
                              :population-size 200,
                              :maximum-random-solution-proportion 0.0},
                             :generation-number 708,
                             :last-improved-generation-number 507,
                             :ratings
                             ([#{{:cocoa-key-code 2, :character "a"}
                                 {:cocoa-key-code 40, :character "e"}
                                 {:cocoa-key-code 33, :character "x"}
                                 {:cocoa-key-code 9, :character "m"}
                                 {:cocoa-key-code 38, :character "t"}
                                 {:cocoa-key-code 37, :character "s"}
                                 {:cocoa-key-code 46, :character "d"}
                                 {:cocoa-key-code 7, :character "o"}
                                 {:cocoa-key-code 4, :character "r"}
                                 {:cocoa-key-code 41, :character "h"}
                                 {:cocoa-key-code 12, :character "z"}
                                 {:cocoa-key-code 0, :character "g"}
                                 {:cocoa-key-code 17, :character "v"}
                                 {:cocoa-key-code 1, :character "i"}
                                 {:cocoa-key-code 3, :character "n"}
                                 {:cocoa-key-code 5, :character "l"}
                                 {:cocoa-key-code 39, :character "y"}
                                 {:cocoa-key-code 16, :character "b"}
                                 {:cocoa-key-code 15, :character "ä"}
                                 {:cocoa-key-code 34, :character "u"}
                                 {:cocoa-key-code 14, :character "q"}
                                 {:cocoa-key-code 45, :character "f"}
                                 {:cocoa-key-code 6, :character "w"}
                                 {:cocoa-key-code 32, :character "k"}
                                 {:cocoa-key-code 11, :character "j"}
                                 {:cocoa-key-code 35, :character "ö"}
                                 {:cocoa-key-code 13, :character "å"}
                                 {:cocoa-key-code 31, :character "p"}
                                 {:cocoa-key-code 8, :character "c"}}
                               1.0085179971004363]
                              [#{{:cocoa-key-code 2, :character "a"}
                                 {:cocoa-key-code 40, :character "e"}
                                 {:cocoa-key-code 9, :character "m"}
                                 {:cocoa-key-code 38, :character "t"}
                                 {:cocoa-key-code 37, :character "s"}
                                 {:cocoa-key-code 46, :character "d"}
                                 {:cocoa-key-code 7, :character "o"}
                                 {:cocoa-key-code 4, :character "r"}
                                 {:cocoa-key-code 41, :character "h"}
                                 {:cocoa-key-code 12, :character "z"}
                                 {:cocoa-key-code 0, :character "g"}
                                 {:cocoa-key-code 17, :character "v"}
                                 {:cocoa-key-code 1, :character "i"}
                                 {:cocoa-key-code 3, :character "n"}
                                 {:cocoa-key-code 5, :character "l"}
                                 {:cocoa-key-code 39, :character "y"}
                                 {:cocoa-key-code 16, :character "b"}
                                 {:cocoa-key-code 15, :character "ä"}
                                 {:cocoa-key-code 34, :character "u"}
                                 {:cocoa-key-code 14, :character "q"}
                                 {:cocoa-key-code 45, :character "f"}
                                 {:cocoa-key-code 6, :character "w"}
                                 {:cocoa-key-code 32, :character "k"}
                                 {:cocoa-key-code 11, :character "j"}
                                 {:cocoa-key-code 13, :character "å"}
                                 {:cocoa-key-code 31, :character "p"}
                                 {:cocoa-key-code 8, :character "c"}
                                 {:cocoa-key-code 33, :character "ö"}
                                 {:cocoa-key-code 35, :character "x"}}
                               1.0085179971004363]
                              [#{{:cocoa-key-code 2, :character "a"}
                                 {:cocoa-key-code 40, :character "e"}
                                 {:cocoa-key-code 33, :character "x"}
                                 {:cocoa-key-code 9, :character "m"}
                                 {:cocoa-key-code 38, :character "t"}
                                 {:cocoa-key-code 37, :character "s"}
                                 {:cocoa-key-code 46, :character "d"}
                                 {:cocoa-key-code 7, :character "o"}
                                 {:cocoa-key-code 4, :character "r"}
                                 {:cocoa-key-code 41, :character "h"}
                                 {:cocoa-key-code 0, :character "g"}
                                 {:cocoa-key-code 17, :character "v"}
                                 {:cocoa-key-code 1, :character "i"}
                                 {:cocoa-key-code 3, :character "n"}
                                 {:cocoa-key-code 5, :character "l"}
                                 {:cocoa-key-code 39, :character "y"}
                                 {:cocoa-key-code 16, :character "b"}
                                 {:cocoa-key-code 15, :character "ä"}
                                 {:cocoa-key-code 34, :character "u"}
                                 {:cocoa-key-code 12, :character "å"}
                                 {:cocoa-key-code 45, :character "f"}
                                 {:cocoa-key-code 6, :character "w"}
                                 {:cocoa-key-code 13, :character "q"}
                                 {:cocoa-key-code 32, :character "k"}
                                 {:cocoa-key-code 11, :character "j"}
                                 {:cocoa-key-code 35, :character "z"}
                                 {:cocoa-key-code 31, :character "p"}
                                 {:cocoa-key-code 8, :character "c"}
                                 {:cocoa-key-code 14, :character "ö"}}
                               1.0085412227013528]
                              [#{{:cocoa-key-code 2, :character "a"}
                                 {:cocoa-key-code 40, :character "e"}
                                 {:cocoa-key-code 9, :character "m"}
                                 {:cocoa-key-code 38, :character "t"}
                                 {:cocoa-key-code 37, :character "s"}
                                 {:cocoa-key-code 46, :character "d"}
                                 {:cocoa-key-code 7, :character "o"}
                                 {:cocoa-key-code 4, :character "r"}
                                 {:cocoa-key-code 33, :character "z"}
                                 {:cocoa-key-code 41, :character "h"}
                                 {:cocoa-key-code 0, :character "g"}
                                 {:cocoa-key-code 17, :character "v"}
                                 {:cocoa-key-code 1, :character "i"}
                                 {:cocoa-key-code 3, :character "n"}
                                 {:cocoa-key-code 5, :character "l"}
                                 {:cocoa-key-code 39, :character "y"}
                                 {:cocoa-key-code 16, :character "b"}
                                 {:cocoa-key-code 15, :character "ä"}
                                 {:cocoa-key-code 34, :character "u"}
                                 {:cocoa-key-code 12, :character "å"}
                                 {:cocoa-key-code 45, :character "f"}
                                 {:cocoa-key-code 6, :character "w"}
                                 {:cocoa-key-code 13, :character "q"}
                                 {:cocoa-key-code 32, :character "k"}
                                 {:cocoa-key-code 11, :character "j"}
                                 {:cocoa-key-code 31, :character "p"}
                                 {:cocoa-key-code 8, :character "c"}
                                 {:cocoa-key-code 14, :character "ö"}
                                 {:cocoa-key-code 35, :character "x"}}
                               1.0085412227013528]
                              [#{{:cocoa-key-code 2, :character "a"}
                                 {:cocoa-key-code 40, :character "e"}
                                 {:cocoa-key-code 9, :character "m"}
                                 {:cocoa-key-code 38, :character "t"}
                                 {:cocoa-key-code 37, :character "s"}
                                 {:cocoa-key-code 46, :character "d"}
                                 {:cocoa-key-code 7, :character "o"}
                                 {:cocoa-key-code 4, :character "r"}
                                 {:cocoa-key-code 33, :character "z"}
                                 {:cocoa-key-code 41, :character "h"}
                                 {:cocoa-key-code 0, :character "g"}
                                 {:cocoa-key-code 17, :character "v"}
                                 {:cocoa-key-code 1, :character "i"}
                                 {:cocoa-key-code 3, :character "n"}
                                 {:cocoa-key-code 5, :character "l"}
                                 {:cocoa-key-code 39, :character "y"}
                                 {:cocoa-key-code 16, :character "b"}
                                 {:cocoa-key-code 15, :character "ä"}
                                 {:cocoa-key-code 34, :character "u"}
                                 {:cocoa-key-code 45, :character "f"}
                                 {:cocoa-key-code 6, :character "w"}
                                 {:cocoa-key-code 12, :character "q"}
                                 {:cocoa-key-code 32, :character "k"}
                                 {:cocoa-key-code 11, :character "j"}
                                 {:cocoa-key-code 13, :character "å"}
                                 {:cocoa-key-code 31, :character "p"}
                                 {:cocoa-key-code 8, :character "c"}
                                 {:cocoa-key-code 14, :character "ö"}
                                 {:cocoa-key-code 35, :character "x"}}
                               1.0086570895820686])})

(view/refresh-view!)
