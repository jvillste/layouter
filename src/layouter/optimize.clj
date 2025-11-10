(ns layouter.optimize
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]
   [layouter.keyboard :as keyboard]
   [layouter.layout :as layout]
   [layouter.rating :as rating]
   [layouter.text :as text]
   [layouter.view :as view]
   [medley.core :as medley])
  (:import
   (java.util Random)))

(defn create-random-double-function
  ([] (create-random-double-function nil))
  ([seed] (let [random (if (some? seed)
                         (Random. seed)
                         (Random.))]
            (fn ([minimum maximum]
                 (+ minimum
                    (* (.nextDouble random)
                       (- maximum minimum))))
              ([]
               (.nextDouble random))))))

(def ^:dynamic random-double (create-random-double-function))

(defn pick-random [collection]
  (nth collection
       (* (random-double)
          (count collection))))

(defn mutate-layout [layout]
  (let [layout-vector (vec layout)
        mapping-1 (pick-random layout-vector)
        mapping-2 (pick-random (remove #{mapping-1}
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

(defn random-layout
  ([]
   (random-layout text/finnish-characters))
  ([characters]
   (loop [remaining-cocoa-key-codes (sort (map :cocoa-key-code (remove :disabled? keyboard/keyboard-keys)))
          remaining-characters characters
          layout #{}]
     (if (empty? remaining-characters)
       (set/union layout
                  (into #{}
                        (for [cocoa-key-code remaining-cocoa-key-codes]
                          {:character ""
                           :cocoa-key-code cocoa-key-code})))
       (let [character (first remaining-characters)
             cocoa-key-code (pick-random remaining-cocoa-key-codes)]
         (recur (remove #{cocoa-key-code} remaining-cocoa-key-codes)
                (rest remaining-characters)
                (conj layout {:character character
                              :cocoa-key-code cocoa-key-code})))))))

(defmacro with-fixed-random-seed [& body]
  `(binding [random-double (create-random-double-function 1)]
     ~@body))



(defn crossbreed-layouts [layout-1 layout-2]
  (loop [cocoa-key-code-to-character-1 (layouter.layout/layout-to-cocoa-key-code-to-character layout-1)
         cocoa-key-code-to-character-2 (layouter.layout/layout-to-cocoa-key-code-to-character layout-2)
         new-layout #{}
         cocoa-key-codes (map :cocoa-key-code layout-1)]
    (if (empty? cocoa-key-codes)
      new-layout
      (let [cocoa-key-code (first cocoa-key-codes)
            character (if (< 0.5 (random-double))
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
         (with-fixed-random-seed
           (crossbreed-layouts #{{:character "a" :cocoa-key-code 0}
                                 {:character "b" :cocoa-key-code 1}}
                               #{{:character "a" :cocoa-key-code 1}
                                 {:character "b" :cocoa-key-code 0}}))))

  (is (= #{{:cocoa-key-code 3, :character "c"}
           {:cocoa-key-code 0, :character "a"}
           {:cocoa-key-code 1, :character "b"}}
         (with-fixed-random-seed
           (crossbreed-layouts #{{:character "a" :cocoa-key-code 0}
                                 {:character "b" :cocoa-key-code 1}
                                 {:character "c" :cocoa-key-code 3}}
                               #{{:character "a" :cocoa-key-code 3}
                                 {:character "b" :cocoa-key-code 0}
                                 {:character "c" :cocoa-key-code 1}})))))

(defn swap-mappings [layout mapping-1 mapping-2]
  (-> layout
      (disj mapping-1
            mapping-2)
      (conj (assoc mapping-1 :cocoa-key-code (:cocoa-key-code mapping-2)))
      (conj (assoc mapping-2 :cocoa-key-code (:cocoa-key-code mapping-1)))))

(defn gradient-descent-one [text-statistics layout]
  (assert (not (nil? layout)))
  (let [current-effort (rating/rate-layout text-statistics layout)
        {:keys [effort layout]} (->> (for [mapping-1 layout
                                           mapping-2 layout]
                                       (-> layout
                                           (swap-mappings mapping-1
                                                          mapping-2)))
                                     (pmap (fn [layout]
                                             {:effort (rating/rate-layout text-statistics
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
                          ((layout/layout-to-character-to-cocoa-key-code layout/qwerty) character))
        qwerty-character (fn [cocoa-key-code]
                           ((layout/layout-to-cocoa-key-code-to-character layout/qwerty) cocoa-key-code))
        make-readable (fn [layout]
                        (sort-by :character
                                 (map (fn [mapping]
                                        {:character (:character mapping)
                                         :qwerty-character (qwerty-character (:cocoa-key-code mapping))})
                                      layout)))]

    (is (= '({:character "a", :qwerty-character "f"}
             {:character "b", :qwerty-character "q"}
             {:character "c", :qwerty-character "e"}
             {:character "x", :qwerty-character "w"}
             {:character "y", :qwerty-character "j"}
             {:character "z", :qwerty-character "k"})
           (make-readable (gradient-descent-all (text/text-statistics "abc" ["a" "b" "c"])
                                                #{{:character "a" :cocoa-key-code (qwerty-key-code "q")}
                                                  {:character "b" :cocoa-key-code (qwerty-key-code "w")}
                                                  {:character "c" :cocoa-key-code (qwerty-key-code "e")}
                                                  {:character "x" :cocoa-key-code (qwerty-key-code "f")}
                                                  {:character "y" :cocoa-key-code (qwerty-key-code "j")}
                                                  {:character "z" :cocoa-key-code (qwerty-key-code "k")}}))))

    (is (= '({:character "a", :qwerty-character "q"}
             {:character "b", :qwerty-character "f"}
             {:character "c", :qwerty-character "e"}
             {:character "x", :qwerty-character "w"}
             {:character "y", :qwerty-character "j"}
             {:character "z", :qwerty-character "k"})
           (make-readable
            (gradient-descent-one (text/text-statistics "abc" ["a" "b" "c"])
                                  #{{:character "a" :cocoa-key-code (qwerty-key-code "q")}
                                    {:character "b" :cocoa-key-code (qwerty-key-code "w")}
                                    {:character "c" :cocoa-key-code (qwerty-key-code "e")}
                                    {:character "x" :cocoa-key-code (qwerty-key-code "f")}
                                    {:character "y" :cocoa-key-code (qwerty-key-code "j")}
                                    {:character "z" :cocoa-key-code (qwerty-key-code "k")}}))))))



(defn best-rating [ratings]
  (assert (not (empty? ratings)))
  (first (sort (map second ratings))))

(def ^:dynamic generation-size 50)
(def ^:dynamic number-of-genrations 1000)
(def ^:dynamic maximum-number-of-generations-without-improvement 1000)

(defonce running-atom? (atom true))
(defonce optimization-state-atom (atom {}))

(defn weighted-random [distribution temperature]
  (let [temperature-adjusted-distribution (map (fn [[value propability]]
                                                 [value (Math/pow propability
                                                                  (/ 1.0 temperature))])
                                               distribution)
        total (reduce + (map second temperature-adjusted-distribution))
        propability-limit (* total
                             (random-double))]
    (loop [sum 0
           [[value propability] & rest] temperature-adjusted-distribution]
      (if (<= (+ sum propability)
              propability-limit)
        (recur (+ sum propability) rest)
        value))))

(deftest test-weighted-random
  (is (= :a
         (binding [random-double (create-random-double-function 1)]
           (weighted-random [[:a 0.7]
                             [:b 0.3]]
                            0.1))))

  (is (= :b
         (binding [random-double (create-random-double-function 1)]
           (weighted-random [[:a 0.7]
                             [:b 0.3]]
                            100)))))

(comment
  (frequencies (repeatedly 1000 #(weighted-random [[:a 0.7]
                                                   [:b 0.3]]
                                                  0.3)))
  )

(defn invert-distribution [distribution]
  (map (fn [[value propability]]
         [value (/ 1.0
                   (+ propability 1e-9))])
       distribution))

(deftest test-invert-distribution
  (is (= '([:a 0.19999999996]
           [:b 0.09999999999])
         (invert-distribution [[:a 5]
                               [:b 10]]))))

(defn normalize-distribution [distribution]
  (let [total (reduce + (map second distribution))]
    (map (fn [[value propability]]
           [value (/ propability
                     total)])
         distribution)))

(deftest test-normalize-distribution
  (is (= '([:a 1/3] [:b 2/3])
         (normalize-distribution [[:a 5]
                                  [:b 10]]))))

(defn ratings-to-distribution [ratings]
  (try (->> ratings
            (invert-distribution)
            (normalize-distribution))
       (catch Throwable throwable
         (def ratings ratings) ;; TODO: remove me
         )))

(deftest test-ratings-to-distribution
  (is (= '([:a 0.6666666666444444]
           [:b 0.33333333335555554])
         (ratings-to-distribution [[:a 5]
                                   [:b 10]]))))

(deftest test-ratings-to-distribution
  (is (= '([:b 0.5999999999200001]
           [:a 0.40000000008000003])
         (ratings-to-distribution #{[:a 1.5]
                                    [:b 1.0]}))))

(defn layouts-to-ratings [text-statistics layouts]
  (pmap (fn [layout]
          [layout (rating/rate-layout text-statistics layout)])
        layouts))

(defn next-generation-ratings [current-generation-ratings
                               solutions-to-ratings
                               random-solution
                               crossbreed
                               mutate-solution
                               {:keys [population-size
                                       elite-proportion
                                       parent-selection-temperature
                                       mutation-propability
                                       random-solution-proportion]}]
  (let [elite-count (max 1 (int (Math/ceil (* population-size elite-proportion))))
        random-solution-count (max 1 (int (Math/ceil (* population-size random-solution-proportion))))
        distribution (ratings-to-distribution current-generation-ratings)]
    (concat (take elite-count
                  (distinct (sort-by second
                                     current-generation-ratings)))
            (->> (repeatedly random-solution-count
                             random-solution)
                 (solutions-to-ratings))
            (->> (fn []
                   (let [child (crossbreed (weighted-random distribution parent-selection-temperature)
                                           (weighted-random distribution parent-selection-temperature))]
                     (if (< (random-double)
                            mutation-propability)
                       (mutate-solution child)
                       child)))
                 (repeat (- population-size
                            elite-count
                            random-solution-count))
                 (apply pcalls)
                 (solutions-to-ratings)))))

(deftest test-next-generation-ratings
  (is (= 5 (count
            (with-fixed-random-seed
              (sort-by second
                       (next-generation-ratings (->> (repeatedly 5 random-layout)
                                                     (layouts-to-ratings text/hybrid-statistics)
                                                     (doall))
                                                (partial layouts-to-ratings text/hybrid-statistics)
                                                random-layout
                                                crossbreed-layouts
                                                mutate-layout
                                                {:population-size 5
                                                 :elite-proportion 0.2
                                                 :parent-selection-temperature 1
                                                 :mutation-propability 0.5
                                                 :random-solution-proportion 0.1})))))))

(defn linear-mapping [base minimum maximum slope x]
  (min maximum
       (max minimum
            (+ base (* slope x)))))

(defn linear-mapping-saturation [base minimum maximum slope]
  (assert (or (and (< 0 slope)
                   (< base maximum))
              (and (> 0 slope)
                   (> base minimum))))

  (int (Math/ceil (Math/abs (/ (- (if (< 0 slope)
                                    maximum
                                    minimum)
                                  base)
                               slope)))))

(defn run-linear-mapping-saturation-test [base minimum maximum slope]
  (is (= (if (< 0 slope)
           maximum
           minimum)
         (int (linear-mapping base minimum maximum slope
                              (linear-mapping-saturation base minimum maximum slope))))))

(deftest test-linear-mapping-saturation
  (run-linear-mapping-saturation-test 1 2 2 0.1)
  (run-linear-mapping-saturation-test 1 4 2 0.1)
  (run-linear-mapping-saturation-test 1 4 4 0.1)
  (run-linear-mapping-saturation-test 1 -4 4 -0.1)

  (is (thrown? AssertionError (linear-mapping-saturation 1 1 1 0.1)))
  (is (thrown? AssertionError (linear-mapping-saturation 1 2 2 -0.1)))
  (is (thrown? AssertionError (linear-mapping-saturation 1 2 0 0.1))))

(def default-metaparameters {:population-size 500
                             :elite-proportion-slope 0.01
                             :minimum-elite-proportion 0.05
                             :maximum-elite-proportion 0.15
                             :minimum-parent-selection-temperature 1.0
                             :maximum-parent-selection-temperature 10.0
                             :parent-selection-temperature-slope 0.01
                             :mutation-propability-slope 0.01
                             :minimum-mutation-propability 0.05
                             :maximum-mutation-propability 0.5
                             :random-solution-proportion-slope 0.01
                             :minimum-random-solution-proportion 0.05
                             :maximum-random-solution-proportion 0.5})

(defn random-metaparameters []
  {:population-size 500

   :elite-proportion-slope (random-double 0.005 0.05)
   :minimum-elite-proportion (random-double 0.01 0.10)
   :maximum-elite-proportion (random-double 0.10 0.20)

   :minimum-parent-selection-temperature (random-double 0.80 1.20)
   :maximum-parent-selection-temperature (random-double 2.0 10.0)
   :parent-selection-temperature-slope (random-double 0.005 0.05)

   :mutation-propability-slope (random-double 0.005 0.05)
   :minimum-mutation-propability (random-double 0.01 0.05)
   :maximum-mutation-propability (random-double 0.1 0.5)

   :random-solution-proportion-slope (random-double 0.005 0.05)
   :minimum-random-solution-proportion (random-double 0.01 0.1)
   :maximum-random-solution-proportion (random-double 0.2 0.7)})

(defn metaparameter-saturation-interval [metaparameters]
  (max (/ 1 (:elite-proportion-slope metaparameters))
       (/ 1 (:parent-selection-temperature-slope metaparameters))
       (/ 1 (:mutation-propability-slope metaparameters))
       (/ 1 (:random-solution-proportion-slope metaparameters))))

(deftest test-metaparameter-saturation-interval
  (is (=
         (metaparameter-saturation-interval ))))

(defn arithmetic-mean [value-1 value-2]
  (/ (+ value-1 value-2)
     2))

(defn crossbreed-metaparameters [metaparameters-1 metaparameters-2]
  (merge-with arithmetic-mean metaparameters-1 metaparameters-2))

(deftest test-crossbreed-metaparameters
  (is (= {:population-size 300}
         (crossbreed-metaparameters {:population-size 100}
                                    {:population-size 500}))))

(defn mutate-metaparameters [metaparameters]
  (medley/map-vals (fn [value]
                     (+ value
                        (* value
                           0.2
                           (pick-random [-1 1])
                           (random-double))))
                   metaparameters))

(deftest test-mutate-metaparameters
  (is (= {:a 0.5410080811492202, :b 466.7282944040489}
         (with-fixed-random-seed
           (mutate-metaparameters {:a 0.5
                                   :b 500})))))

(defn next-generation-parameters [generations-since-last-improvement & [metaparameters]]
  (let [{:keys [population-size
                elite-proportion-slope
                minimum-elite-proportion
                maximum-elite-proportion
                minimum-parent-selection-temperature
                maximum-parent-selection-temperature
                parent-selection-temperature-slope
                mutation-propability-slope
                minimum-mutation-propability
                maximum-mutation-propability
                random-solution-proportion-slope
                minimum-random-solution-proportion
                maximum-random-solution-proportion]}
        metaparameters]

    {:population-size population-size

     :elite-proportion (linear-mapping maximum-elite-proportion
                                       minimum-elite-proportion
                                       maximum-elite-proportion
                                       (- elite-proportion-slope)
                                       generations-since-last-improvement)

     :parent-selection-temperature (linear-mapping minimum-parent-selection-temperature
                                                   minimum-parent-selection-temperature
                                                   maximum-parent-selection-temperature
                                                   parent-selection-temperature-slope
                                                   generations-since-last-improvement)

     :mutation-propability (linear-mapping minimum-mutation-propability
                                           minimum-mutation-propability
                                           maximum-mutation-propability
                                           mutation-propability-slope
                                           generations-since-last-improvement)

     :random-solution-proportion (linear-mapping minimum-random-solution-proportion
                                                 minimum-random-solution-proportion
                                                 maximum-random-solution-proportion
                                                 random-solution-proportion-slope
                                                 generations-since-last-improvement)}))

(deftest test-next-generation-parameters
  (is (= {:population-size 500,
          :elite-proportion 0.13999999999999999,
          :parent-selection-temperature 1.01,
          :mutation-propability 0.060000000000000005,
          :random-solution-proportion 0.060000000000000005}
         (next-generation-parameters 1)))

  (is (= {:population-size 2,
          :elite-proportion 0.13999999999999999,
          :parent-selection-temperature 1.01,
          :mutation-propability 0.060000000000000005,
          :random-solution-proportion 0.060000000000000005}
         (next-generation-parameters 1 {:population-size 2}))))

(defonce metaoptimization-history-atom (atom []))
(defonce optimization-history-atom (atom []))
(defonce stop-requested?-atom (atom false))

(defn optimize [random-solution
                crossbreed
                rate-solutions
                mutate-solution
                & [{:keys [initial-ratings
                           metaparameters
                           number-of-generations
                           history-atom
                           logging-frequency]
                    :or {history-atom optimization-history-atom
                         logging-frequency 1}}]]
  (reset! history-atom [])
  #_(reset! stop-requested?-atom false)

  (println "starting optimization")

  #_(println "metaparameters" (pr-str metaparameters))

  (let [metaparameters (merge default-metaparameters
                              metaparameters)]
    (loop [state {:metaparameters metaparameters
                  :generation-number 0
                  :last-improved-generation-number 0
                  :ratings (concat initial-ratings
                                   (->> (repeatedly (- (:population-size metaparameters)
                                                       (count initial-ratings))
                                                    random-solution)
                                        (rate-solutions)))}]

      (swap! history-atom
             (fn [states]
               (if (< (count states)
                      2)
                 (conj states state)
                 (concat (drop-last states)
                         [(-> (last states)
                              (assoc :best-rating (best-rating (:ratings state)))
                              (dissoc :ratings))
                          state]))))

      (when (= 0 (mod (:generation-number state)
                      20))
        ;; (println "calling refresh in " (.getName (Thread/currentThread)))
        (view/refresh-view!))

      (when (= 0 (mod (:generation-number state)
                      logging-frequency))
        (println "generation:" (:generation-number state) "/" (when number-of-generations
                                                                (dec number-of-generations))
                 "best rating: " (best-rating (:ratings state)))
        ;; (prn (merge (-> state
        ;;                 (dissoc :ratings)
        ;;                 (assoc :best-rating (best-rating (:ratings state))))
        ;;             (next-generation-parameters (- (:generation-number state)
        ;;                                            (:last-improved-generation-number state)))))
        )

      (let [generations-since-latest-improvement (- (:generation-number state)
                                                    (:last-improved-generation-number state))
            reset-needed? (= 1000 generations-since-latest-improvement)
            next-generation-ratings (next-generation-ratings (:ratings state)
                                                             rate-solutions
                                                             random-solution
                                                             crossbreed
                                                             mutate-solution
                                                             (if reset-needed?
                                                               (do (println "resetting population due to stagnation")
                                                                   {:population-size (:population-size metaparameters)
                                                                    :elite-proportion 0.1
                                                                    :parent-selection-temperature 1.0
                                                                    :mutation-propability 0
                                                                    :random-solution-proportion 0.9})
                                                               (next-generation-parameters generations-since-latest-improvement
                                                                                           metaparameters)))]

        (if (or (and (some? number-of-generations)
                     (= (dec number-of-generations)
                        (:generation-number state)))
                @stop-requested?-atom)
          (do (println "stopped")
              state)
          (recur (assoc state
                        :generation-number (inc (:generation-number state))
                        :last-improved-generation-number (if (or reset-needed?
                                                                 (< (best-rating next-generation-ratings)
                                                                    (best-rating (:ratings state))))
                                                           (:generation-number state)
                                                           (:last-improved-generation-number state))
                        :ratings next-generation-ratings)))))))
