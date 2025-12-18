(ns layouter.optimization-progress-view
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [flow-gl.gui.path :as path]
   [fungl.color :as color]
   [fungl.layouts :as layouts]
   [layouter.gui :as gui]
   [layouter.layout :as layout]
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
  (is (= '({:x 0.0, :y 0.0}
           {:x 10.0, :y 10.0})
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

(defn scale-to-view-but-preserve-full-y-scale [width height points]
  (if (empty? points)
    points
    (map (partial scale-point {:x (double (/ width
                                             (max 1e-9
                                                  (- (apply max (map :x points))
                                                     (apply min (map :x points))))))
                               :y (double (/ height
                                             (max 1e-9
                                                  (apply max (map :y points)))))})
         points)))

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

(defn move-x-to-origin [points]
  (if (empty? points)
    points
    (map (partial add-points
                  {:x (- (apply min (map :x points)))
                   :y 0})
         points)))

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

(defn- progress-graph-view [displayed-keys enriched-states key-to-color graph-height]
  (apply layouts/superimpose
         (for [key displayed-keys]
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
                                 (move-to-origin)))))))))



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
                            ;; :layout-diversity
                            :layout-entropy

                            ;; :elite-proportion
                            ;; :parent-selection-temperature
                            ;; :mutation-propability
                            ;; :random-solution-proportion
                            :generations-since-last-improvement
                            ]
            key-to-color (gui/create-key-to-color 0.5 0.5 displayed-keys)
            graph-height 800]

        (apply layouts/vertically-2
               {:margin 10}
               (progress-graph-view (->> displayed-keys
                                         (remove #{:generation-number
                                                   ;; :generations-since-last-improvement
                                                   }))
                                    enriched-states
                                    key-to-color
                                    graph-height)

               (for [key displayed-keys]
                 (gui/text (let [values (remove nil? (map key enriched-states))]
                             (str key " " (key (last enriched-states))
                                  (let [])
                                  (when (not (empty? values))
                                    (str " min: " (apply min values)))
                                  (when (not (empty? values))
                                    (str " max: " (apply max values)))))
                           {:color (key-to-color key)}))

               #_(let [metaparameters (:metaparameters (last enriched-states))]
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


(defn rated-metaparameters [log-file-path]
  (if (not (.exists (io/file log-file-path)))
    []
    (->> #_(tail 0 log-file-path)
         (optimize/read-log log-file-path)
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

(def metaparameter-optimization-log-file-path "temp/metaparameter-optimization-log.edn")



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


(defn cluster-numbers-by-relative-difference [maximum-relative-difference numbers]
  (let [sorted-numbers (sort numbers)]
    (reduce (fn [clusters number]
              (let [current-cluster (peek clusters)]
                (if (> (/ number
                          (first current-cluster))
                       maximum-relative-difference)
                  (conj clusters [number])
                  (conj (pop clusters)
                        (conj current-cluster number)))))
            [[(first sorted-numbers)]]
            (rest sorted-numbers))))

(deftest test-cluster-numbers-by-relative-difference
  (is (= [[1 1.005 1.008 1.01]
          [1.011]
          [1.2]]
         (cluster-numbers-by-relative-difference 1.01 [1 1.005 1.008 1.01 1.011 1.2]))))

(defn cluster-items-by-relative-difference [maximum-relative-difference get-number items]
  (if (empty? items)
    []
    (let [sorted-items (sort-by get-number items)]
      (reduce (fn [clusters item]
                (let [current-cluster (peek clusters)]
                  (if (> (/ (get-number item)
                            (get-number (first current-cluster)))
                         maximum-relative-difference)
                    (conj clusters [item])
                    (conj (pop clusters)
                          (conj current-cluster item)))))
              [[(first sorted-items)]]
              (rest sorted-items)))))

(deftest test-cluster-items-by-relative-difference
  (is (= []
         (cluster-items-by-relative-difference 1.01 :x [])))

  (is (= [[{:x 1} {:x 1.005} {:x 1.008} {:x 1.01}]
          [{:x 1.011}]]
         (cluster-items-by-relative-difference 1.01 :x [{:x 1} {:x 1.005} {:x 1.008} {:x 1.01} {:x 1.011}]))))

(defn sample-items-by-distance [minimum-distance measure-distance items]
  (reduce (fn [chosen-items item]
            (let [latest-chosen-item (peek chosen-items)]
              (if (> (measure-distance item
                                       latest-chosen-item)
                     minimum-distance)
                (conj chosen-items item)
                chosen-items)))
          [(first items)]
          (rest items)))

(deftest test-sample-items-by-distance
  (is (= [0 4 8]
         (sample-items-by-distance 3 - (range 10)))))

(defn best-ratings-per-statistics-and-multipliers [layout-optimization-log]
  (->> layout-optimization-log
       ;; (take 1)
       (group-by (juxt :text-statistics-name :multipliers))
       (medley/map-vals (fn [states]
                          (->> states
                               (mapcat :ratings)
                               (sort-by second)
                               (first)
                               (second))))))

(defn best-layouts-per-statistics-and-multipliers [number-of-layouts-per-group minimum-number-of-differing-keys log]
  (->> log
       (group-by (juxt :text-statistics-name :multipliers))
       (medley/map-vals (fn [states]
                          (->> states
                               (mapcat :ratings)
                               (sort-by second)
                               (map first)
                               (distinct)
                               (sample-items-by-distance minimum-number-of-differing-keys layout/number-of-differing-keys)
                               (take number-of-layouts-per-group)
                               (map (fn [layout]
                                      (assoc (select-keys (first states)
                                                          [:text-statistics-name :multipliers])
                                             :layout layout))))))
       (vals)
       (apply concat)))


(defn last-to-first [function & arguments]
  (apply function
         (last arguments)
         (drop-last arguments)))

(defn ratings-from-layout-optimization-log [text-statistics multipliers layout-optimization-log]
  (->> layout-optimization-log
       (group-by (juxt :text-statistics-name :multipliers))
       (last-to-first get
                      [(:name text-statistics)
                       multipliers])
       (mapcat :ratings)))

(def emphasize-roll-key-and-vertical-movement-multipliers {:digram-roll 1,
                                                           :trigram-roll 1,

                                                           :horizontal-movement 1,
                                                           :vertical-movement 1,
                                                           :vertical-movement-in-skipgram 1

                                                           :key-rating 0.5,
                                                           :finger-type 0.5,

                                                           :hand-balance 0.1
                                                           :hand-alternation 0.0
                                                           :dist-from-colemak 0.1})

(defn optimize-layout []
  (let [text-statistics
        #_text/finnish-statistics
        #_text/finnish-statistics-without-å
        #_text/english-statistics
        #_text/hybrid-statistics
        #_text/hybrid-statistics-without-å
        text/keyboard-design-com-english-text-statistics
        #_(:fi key-log/statistics-from-key-log)
        #_(:en key-log/statistics-from-key-log)
        #_(:hybrid key-log/statistics-from-key-log)

        multipliers

        ;; hand alternating
        ;; {:key-rating 0.5,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 0.0,
        ;;  :hand-balance 0.1,
        ;;  :hand-alternation 1,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.1,
        ;;  :digram-roll 0.0,
        ;;  :horizontal-movement 1}

        ;; hand alternating, more key weight
        ;; {:key-rating 1
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 0.0,
        ;;  :hand-balance 0.1,
        ;;  :hand-alternation 1,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.1,
        ;;  :digram-roll 0.0,
        ;;  :horizontal-movement 1}

        ;; hand alternating, more key and finger weight
        ;; {:trigram-roll 0.0,
        ;;  :digram-roll 0.0,

        ;;  :key-rating 1
        ;;  :finger-type 1,

        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :horizontal-movement 1

        ;;  :hand-balance 0.1,
        ;;  :hand-alternation 1,

        ;;  :dist-from-colemak 0.0}


        ;; rolls and alternation
        ;; {:key-rating 0.5,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 0.1,
        ;;  :hand-alternation 1,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.1,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}

        ;; rolls, no alternation, finger 0.1 key 0.5
        ;; {:key-rating 0.5,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 0.1,
        ;;  :hand-alternation 0.0,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.1,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}

        ;; rolls, no alternation, finger 1 key 0.5
        ;; {:key-rating 0.5,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 0.1,
        ;;  :dist-from-colemak 0.1,
        ;;  :finger-type 1,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}


        ;; best rolls
        ;; {:key-rating 0.1,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 0.1,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.1,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}

        ;; {:key-rating 0.5,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 0.1,
        ;;  :dist-from-colemak 0.1,
        ;;  :finger-type 0.5,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}

        ;; {:key-rating 1,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 0.1,
        ;;  :dist-from-colemak 0.1,
        ;;  :finger-type 0.1,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}
        ;; {:key-rating 1,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 1,
        ;;  :dist-from-colemak 0.1,
        ;;  :finger-type 1,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}



        ;; {:key-rating 0.5,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 1,
        ;;  :hand-balance 0.1,
        ;;  :hand-alternation 0.0,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.1,
        ;;  :digram-roll 1,
        ;;  :horizontal-movement 1}


        ;; no finger no roll all alternation and movement
        ;; {:key-rating 1,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 0.0,
        ;;  :hand-balance 0.0,
        ;;  :hand-alternation 1,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.0,
        ;;  :digram-roll 0.0,
        ;;  :horizontal-movement 1}


        ;; only movement
        ;; {:key-rating 0.0,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 0.0,
        ;;  :hand-balance 0.0,
        ;;  :hand-alternation 0.0,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 0.0,
        ;;  :digram-roll 0.0,
        ;;  :horizontal-movement 1}

        ;; {:key-rating 1.0,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 0.0,
        ;;  :hand-balance 0.0,
        ;;  :hand-alternation 1,
        ;;  :dist-from-colemak 0.0,
        ;;  :finger-type 1.0,
        ;;  :digram-roll 0.0,
        ;;  :horizontal-movement 1}

        ;; {:key-rating 1.0,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 0.0,
        ;;  :hand-balance 0.1,
        ;;  :hand-alternation 1,
        ;;  :finger-type 0.5,
        ;;  :digram-roll 0.0,
        ;;  :horizontal-movement 1,
        ;;  :dist-from-colemak 0.0}

        ;; {:key-rating 1.0,
        ;;  :vertical-movement-in-skipgram 1,
        ;;  :vertical-movement 1,
        ;;  :trigram-roll 0.0,
        ;;  :hand-balance 0.0,
        ;;  :hand-alternation 1,
        ;;  :finger-type 0.5,
        ;;  :digram-roll 0.0,
        ;;  :horizontal-movement 1,
        ;;  :dist-from-colemak 0.0}

        {:key-rating 1.0,
         :vertical-movement-in-skipgram 1,
         :vertical-movement 1,
         :trigram-roll 0.0,
         :hand-balance 0.0,
         :hand-alternation 1,
         :distance-from-colemak 0.0,
         :finger-type 1.0,
         :digram-roll 0.0,
         :horizontal-movement 1}
        ]
    #_(:multipliers @layouter.layout-comparison-view/selected-named-layout-atom)


    (-> (optimize/optimize-layout static-metaparameters
                                  #_emphasize-roll-key-and-vertical-movement-multipliers
                                  multipliers
                                  text-statistics
                                  optimize/layout-optimization-log-file-path
                                  {:maximum-number-of-generations-without-improvement 200})
        (update :ratings (fn [ratings]
                           (let [hill-climbed-layout (optimize/hill-climb-all text-statistics
                                                                              multipliers
                                                                              (->> ratings
                                                                                   (sort-by second)
                                                                                   (first)
                                                                                   (first)))]
                             (->> (conj ratings
                                        [(set hill-climbed-layout)
                                         (rating/rate-layout text-statistics
                                                             hill-climbed-layout
                                                             multipliers)])
                                  (sort-by second))))))))
(comment
  (doto (Thread. (fn []
                   (reset! optimize/stop-requested?-atom false)
                   (optimize/optimize-repeatedly! optimize-layout)))
    (.setName "repeating layout optimization")
    (.start))
  (reset! optimize/stop-requested?-atom true)
  ;; hot-right-now TODO: remove me



  (do (reset! optimize/optimization-history-atom [])
      (reset! optimize/metaoptimization-history-atom []))

  (reset! view/event-channel-atom nil)

  (view/start-view #'metaoptimization-progress-view)

  (view/start-view (fn [] (gui/black-background [optimization-progress-view optimize/optimization-history-atom]))
                   ;;{:join? true}
                   )

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


  (reset! optimize/stop-requested?-atom true)

  (->> (ratings-from-layout-optimization-log text/finnish-statistics
                                             emphasize-roll-key-and-vertical-movement-multipliers
                                             @optimize/layout-optimization-log-atom)
       (sort-by second)
       (first)
       (second))
  ;; => 0.7018095607792347

  (best-ratings-per-statistics-and-multipliers @optimize/layout-optimization-log-atom)
  ;; before second english round
  ;; => {["en"
  ;;      {:digram-roll 1,
  ;;       :trigram-roll 0,
  ;;       :key-rating 1,
  ;;       :finger-type 0.1,
  ;;       :horizontal-movement 0.1,
  ;;       :vertical-movement 1,
  ;;       :hand-balance 0.1}]
  ;;     0.7146674737091715,
  ;;     ["fi"
  ;;      {:digram-roll 1,
  ;;       :trigram-roll 0,
  ;;       :key-rating 1,
  ;;       :finger-type 0.1,
  ;;       :horizontal-movement 0.1,
  ;;       :vertical-movement 1,
  ;;       :hand-balance 0.1}]
  ;;     0.7018095607792347,
  ;;     ["hy"
  ;;      {:digram-roll 1,
  ;;       :trigram-roll 0,
  ;;       :key-rating 1,
  ;;       :finger-type 0.1,
  ;;       :horizontal-movement 0.1,
  ;;       :vertical-movement 1,
  ;;       :hand-balance 0.1}]
  ;;     0.7435772289930073}

  (doto (Thread. (fn []
                   (reset! optimize/stop-requested?-atom false)
                   (let [text-statistics #_text/finnish-statistics text/english-statistics #_text/hybrid-statistics
                         rating-clusters (->> (ratings-from-layout-optimization-log text-statistics
                                                                                    emphasize-roll-key-and-vertical-movement-multipliers
                                                                                    @optimize/layout-optimization-log-atom)
                                              (map (fn [[layout rating]]
                                                     [(layout/finnish-layout-to-finnish-layout-without-å layout) rating]))

                                              (cluster-items-by-relative-difference 1.01 second)
                                              (take 5))]
                     (when (empty? rating-clusters)
                       (println "no ratings to clusterize"))
                     (doseq [[cluster-number rating-cluster] (map vector
                                                                  (range)
                                                                  rating-clusters)]
                       (println "optimizing cluster" (inc cluster-number) "/" (count rating-clusters)
                                "from" (second (first rating-cluster))
                                "to"
                                (second (last rating-cluster)))

                       (optimize/optimize-repeatedly! (fn []
                                                        (optimize-layout static-metaparameters
                                                                         emphasize-roll-key-and-vertical-movement-multipliers
                                                                         text-statistics
                                                                         optimize/layout-optimization-log-file-path
                                                                         {:maximum-number-of-generations-without-improvement 300
                                                                          :initial-ratings rating-cluster}))
                                                      {:maximum-number-of-rounds 2})))))
    (.setName "clusterized optimization")
    (.start))

  (reset! optimize/stop-requested?-atom true)

  ;; hot-right-now TODO: remove me

  ;; remove hybrid statistics from log

  (->> #_@optimize/layout-optimization-log-atom
       (optimize/read-log optimize/layout-optimization-log-file-path)
       (best-ratings-per-statistics-and-multipliers))

  (do (.delete (io/file optimize/layout-optimization-log-file-path))
      (->> #_@optimize/layout-optimization-log-atom
           (optimize/read-log "/Users/jukka/google-drive/src/layouter/temp/layout-optimization-log copy 8.edn"
                              #_optimize/layout-optimization-log-file-path)
           (remove (fn [state]
                     (->> state
                          :ratings
                          (sort-by second)
                          (first)
                          (first)
                          (rating/distance-from-colemak)
                          (> 0.5))))
           #_(count)
           #_(take 2)
           #_(best-ratings-per-statistics-and-multipliers)
           (optimize/write-log optimize/layout-optimization-log-file-path))
      )

  (every? map? (optimize/read-log #_"/Users/jukka/google-drive/src/layouter/temp/layout-optimization-log copy 3.edn"
                                  optimize/layout-optimization-log-file-path))

  (->> @optimize/layout-optimization-log-atom
       (filter (comp :distance-from-colemak
                     :multipliers))
       (count))

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

  (->> @optimize/layout-optimization-log-atom)
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
