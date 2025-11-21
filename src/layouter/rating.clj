(ns layouter.rating
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [layouter.keyboard :as keyboard]
   [layouter.layout :as layout]
   [layouter.text :as text]))


(def finger-hand {0 0
                  1 0
                  2 0
                  3 0
                  4 1
                  5 1
                  6 1
                  7 1})

(def finger-type
  {0 :pinky
   1 :ring
   2 :middle
   3 :index
   4 :index
   5 :middle
   6 :ring
   7 :pinky})

(defn rate-finger-type [key]
  (assoc (case (finger-type (:finger key))
           :index {:effort 0}
           :middle {:effort 0.25}
           :ring {:effort 0.5}
           :pinky {:effort 1})
         :label (finger-type (:finger key))
         :rating :finger-type))

(deftest test-rate-finger-type
  (is (= {:effort 0.25, :label :middle, :rating :finger-type}
         (rate-finger-type {:finger 2}))))

(def key-class-effort {:home 0
                       :easy-index 0.15
                       :regular 0.25
                       :sideways 0.55
                       :middle 1})

(defn key-rating [key]
  {:rating :key
   :label (:class key)
   :effort (key-class-effort (:class key))})

(def ^:dynamic multipliers {:digram-roll 1
                            :trigram-roll 1
                            :key-rating 1
                            :finger-type 1
                            :horizontal-movement 1
                            :vertical-movement 1
                            :hand-balance 1
                            :distance-from-colemak 1})

(defn multiplier [multiplier-key]
  (or (get multipliers multiplier-key)
      1))

(defn multiply-effort [multiplier-key rating]
  (update rating
          :effort
          (partial * (multiplier multiplier-key))))

(defn rate-key [key]
  [(multiply-effort :finger-type (rate-finger-type key))
   (multiply-effort :key-rating (key-rating key))])

(defn hands [key-sequence]
  (set (map (comp finger-hand :finger)
            key-sequence)))

(defn same-hand? [key-pair]
  (= 1 (count (hands key-pair))))

(defn same-finger? [key-pair]
  (= 1 (count (set (map :finger key-pair)))))

(defn different-fingers? [key-sequence]
  (= (count key-sequence)
     (count (set (map :finger key-sequence)))))

(defn same-column? [key-pair]
  (= 1 (count (set (map :column key-pair)))))

(defn left-hand? [key-sequence]
  (= #{0} (hands key-sequence)))

(defn right-hand? [key-sequence]
  (= #{1} (hands key-sequence)))

(defn same-row? [key-sequence]
  (= 1 (count (set (map :row key-sequence)))))

(defn two-row-leap? [key-pair]
  (= #{0 2} (set (map :row key-pair))))

(defn one-row-leap? [key-pair]
  (or (= #{0 1} (set (map :row key-pair)))
      (= #{1 2} (set (map :row key-pair)))))

(defn on-home-row? [key-sequence]
  (= #{1} (set (map :row key-sequence))))

(defn rate-vertical-movement [key-pair]
  (assoc (cond (not (same-hand? key-pair))
               {:label :different-hand
                :effort 0}

               (and (not (same-finger? key-pair))
                    (one-row-leap? key-pair))
               {:label :different-finger-one-row-leap
                :effort 0.1}

               (and (not (same-finger? key-pair))
                    (two-row-leap? key-pair))
               {:label :different-finger-two-row-leap
                :effort 0.2}

               (and (same-finger? key-pair)
                    (one-row-leap? key-pair))
               {:label :same-finger-one-row-leap
                :effort 0.75}

               (and (same-finger? key-pair)
                    (two-row-leap? key-pair))
               {:label :same-finger-two-row-leap
                :effort 1}

               :else
               {:label :same-row
                :effort 0})
         :rating :vertical-movement))

(deftest test-rate-vertical-movement
  (is (= {:label :different-hand, :effort 0, :rating :vertical-movement}
         (rate-vertical-movement [(keyboard/qwerty-character-to-key "f")
                                  (keyboard/qwerty-character-to-key "j")])))

  (is (= {:label :different-finger-one-row-leap, :effort 0.25, :rating :vertical-movement}
         (rate-vertical-movement [(keyboard/qwerty-character-to-key "j")
                                  (keyboard/qwerty-character-to-key "i")])))

  (is (= {:label :different-finger-two-row-leap, :effort 0.5, :rating :vertical-movement}
         (rate-vertical-movement [(keyboard/qwerty-character-to-key "i")
                                  (keyboard/qwerty-character-to-key "m")])))

  (is (= {:label :same-finger-one-row-leap, :effort 0.75, :rating :vertical-movement}
         (rate-vertical-movement [(keyboard/qwerty-character-to-key "j")
                                  (keyboard/qwerty-character-to-key "u")])))

  (is (= {:label :same-finger-two-row-leap, :effort 1, :rating :vertical-movement}
         (rate-vertical-movement [(keyboard/qwerty-character-to-key "m")
                                  (keyboard/qwerty-character-to-key "u")]))))

(defn rate-horizontal-movement [key-pair]
  (assoc (cond (not (same-finger? key-pair))
               {:label :different-finger
                :effort 0}

               (same-column? key-pair)
               {:label :same-column
                :effort 0}

               :else
               {:label :same-finger-different-column
                :effort 1})
         :rating :horizontal-movement))


(defn roll? [key-pair]
  (and (same-hand? key-pair)
       (different-fingers? key-pair)))

(defn ascending? [sequence]
  (= sequence (sort sequence)))

(defn descending? [sequence]
  (= sequence
     (reverse (sort sequence))))

(defn invards-progression? [key-sequence]
  (or (and (left-hand? key-sequence)
           (ascending? (map :finger key-sequence)))
      (and (right-hand? key-sequence)
           (descending? (map :finger key-sequence)))))

(defn invards-roll? [key-sequence]
  (and (roll? key-sequence)
       (invards-progression? key-sequence)))

;; (defn adjacent-fingers? [key-pair]
;;   (= 1 (abs (- (:finger (first key-pair))
;;                (:finger (second key-pair))))))

(defn adjacent-number-pair? [[number-1 number-2]]
  (or (= number-1 number-2)
      (= 1 (abs (- number-1 number-2)))))

(deftest test-adjacent-number-pair?
  (is (adjacent-number-pair? [1 2]))
  (is (adjacent-number-pair? [1 1]))
  (is (not (adjacent-number-pair? [1 3]))))

(defn adjacent-numbers? [numbers]
  (every? adjacent-number-pair?
          (partition 2 1 numbers)))

(deftest test-adjacent-numbers?
  (is (adjacent-numbers? [0
                          1]))

  (is (adjacent-numbers? [0
                          1
                          2]))

  (is (adjacent-numbers? [1
                          0]))

  (is (not (adjacent-numbers? [0
                               2])))

  (is (not (adjacent-numbers? [0
                               1
                               3])))

  (is (not (adjacent-numbers? [2
                               0]))))

(defn adjacent-fingers? [key-sequence]
  (adjacent-numbers? (map :finger key-sequence)))

(defn two-adjacent-rows? [key-sequence]

  (let [rows (map :row key-sequence)]
    (and (adjacent-numbers? rows)
         (= 2 (count (set rows))))))

(deftest test-two-adjacent-rows?
  (is (two-adjacent-rows? [{:row 1}
                           {:row 1}
                           {:row 2}]))

  (is (not (two-adjacent-rows? [{:row 0}
                                {:row 1}
                                {:row 2}])))

  (is (not (two-adjacent-rows? [{:row 0}
                                {:row 2}]))))

(defn rate-n-gram-roll [key-sequence]
  (let [rating (keyword (str (count key-sequence) "-roll"))]
    (if (not (and (same-hand? key-sequence)
                  (different-fingers? key-sequence)
                  (or (ascending? (map :finger key-sequence))
                      (descending? (map :finger key-sequence)))
                  (or (same-row? key-sequence)
                      (two-adjacent-rows? key-sequence))))
      {:rating rating
       :effort 1
       :label :no-roll}
      (let [effort-reductions (-> []
                                  (cond->
                                      (adjacent-fingers?  key-sequence)
                                    (conj {:effort-reduction-label :adjacent
                                           :effort-reduction 2/10})

                                    (not (adjacent-fingers?  key-sequence))
                                    (conj {:effort-reduction-label :unadjacent
                                           :effort-reduction 0})

                                    (invards-progression? key-sequence)
                                    (conj {:effort-reduction-label :invards
                                           :effort-reduction 1/10})

                                    (not (invards-progression? key-sequence))
                                    (conj {:effort-reduction-label :outwards
                                           :effort-reduction 0})

                                    (on-home-row? key-sequence)
                                    (conj {:effort-reduction-label :home-row
                                           :effort-reduction 2/10})

                                    (and (not (on-home-row? key-sequence))
                                         (same-row? key-sequence))
                                    (conj {:effort-reduction-label :same-non-home-row
                                           :effort-reduction 1/20})

                                    (and (not (on-home-row? key-sequence))
                                         (not (same-row? key-sequence))
                                         (two-adjacent-rows? key-sequence))
                                    (conj {:effort-reduction-label :two-adjacent-rows
                                           :effort-reduction 0})))]
        {:rating rating
         :effort (double (- 1/2 (reduce + (remove nil? (map :effort-reduction effort-reductions)))))
         :label (keyword (str (string/join "-" (map (comp name :effort-reduction-label)
                                                    effort-reductions))
                              "-roll"))}))))

(deftest test-rate-n-gram-roll
  (is (= {:rating :3-roll, :effort 0.0, :label :adjacent-invards-home-row-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "a")
                            (keyboard/qwerty-character-to-key "s")
                            (keyboard/qwerty-character-to-key "d")])))

  (is (= {:rating :3-roll, :effort 0.1, :label :adjacent-outwards-home-row-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "f")
                            (keyboard/qwerty-character-to-key "d")
                            (keyboard/qwerty-character-to-key "s")])))

  (is (= {:rating :3-roll, :effort 0.15, :label :adjacent-invards-same-non-home-row-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "z")
                            (keyboard/qwerty-character-to-key "x")
                            (keyboard/qwerty-character-to-key "c")])))

  (is (= {:rating :3-roll, :effort 0.2, :label :unadjacent-invards-home-row-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "a")
                            (keyboard/qwerty-character-to-key "d")
                            (keyboard/qwerty-character-to-key "f")])))

  (is (= {:rating :3-roll, :effort 0.2, :label :adjacent-invards-two-adjacent-rows-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "a")
                            (keyboard/qwerty-character-to-key "e")
                            (keyboard/qwerty-character-to-key "r")])))

  (is (= {:rating :3-roll, :effort 0.3, :label :adjacent-outwards-two-adjacent-rows-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "r")
                            (keyboard/qwerty-character-to-key "e")
                            (keyboard/qwerty-character-to-key "a")])))

  (is (= {:rating :3-roll, :effort 0.3, :label :unadjacent-outwards-home-row-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "f")
                            (keyboard/qwerty-character-to-key "d")
                            (keyboard/qwerty-character-to-key "a")])))

  (is (= {:rating :3-roll, :effort 0.45, :label :unadjacent-outwards-same-non-home-row-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "c")
                            (keyboard/qwerty-character-to-key "x")
                            (keyboard/qwerty-character-to-key "<")])))

  (is (= {:rating :3-roll, :effort 0.35, :label :unadjacent-invards-same-non-home-row-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "<")
                            (keyboard/qwerty-character-to-key "x")
                            (keyboard/qwerty-character-to-key "c")])))



  (is (= {:rating :2-roll, :effort 1, :label :no-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "f")
                            (keyboard/qwerty-character-to-key "j")])))

  (is (= {:rating :3-roll, :effort 1, :label :no-roll}
         (rate-n-gram-roll [(keyboard/qwerty-character-to-key "a")
                            (keyboard/qwerty-character-to-key "e")
                            (keyboard/qwerty-character-to-key "c")]))))

(defn rate-key-pair [key-pair]
  [(multiply-effort :vertical-movement (rate-vertical-movement key-pair))
   (multiply-effort :horizontal-movement (rate-horizontal-movement key-pair))
   (multiply-effort :digram-roll (rate-n-gram-roll key-pair))])

(defn rate-key-triple [key-triple]
  [(multiply-effort :trigram-roll (rate-n-gram-roll key-triple))])

(defn rate-distribution [rate distribution]
  (reduce +
          (for [[value propability] distribution]
            (* propability
               (rate value)))))

(defn total-effort [ratings]
  (apply + (map :effort ratings)))

(defn rate-hand-balance [character-distribution character-to-key]
  (- 1
     (->> (for [[character propability] character-distribution]
            [(-> character
                 (character-to-key)
                 (:finger)
                 (finger-hand))
             propability])
          (map #(apply hash-map %))
          (apply merge-with +)
          (vals)
          (sort)
          (apply /))))

(deftest test-rate-hand-balance
  (is (= 1.0
         (rate-hand-balance {"a" 0.5
                             "b" 0.5}
                            {"a" {:finger 3}
                             "b" {:finger 4}})))

  (is (= 0.8181818181818181
         (rate-hand-balance {"a" 0.45
                             "b" 0.55}
                            {"a" {:finger 3}
                             "b" {:finger 4}})))

  (is (= 0.6666666666666667
         (rate-hand-balance {"a" 0.4
                             "b" 0.6}
                            {"a" {:finger 3}
                             "b" {:finger 4}}))))

(defn distance-from-colemak [layout]
  (layout/layout-distance layout/colemak-dh layout))

(defn rate-layout
  ([layout]
   (rate-layout text/hybrid-statistics layout))

  ([text-statistics layout]
   (assert (and (set? layout)
                (every? :cocoa-key-code layout)))

   (let [character-to-key (comp keyboard/cocoa-key-code-to-key
                                (layout/layout-to-character-to-cocoa-key-code layout))]
     (+ (rate-distribution (fn [digram]
                             (total-effort (rate-key-pair (map character-to-key digram))))
                           (:digram-distribution text-statistics))
        #_(rate-distribution (fn [trigram]
                               (total-effort (rate-key-triple (map character-to-key trigram))))
                             (:trigram-distribution text-statistics))
        (rate-distribution (fn [character]
                             (total-effort (rate-key (character-to-key character))))
                           (:character-distribution text-statistics))
        (* (multiplier :hand-balance)
           (rate-hand-balance (:character-distribution text-statistics)
                              character-to-key))
        (* (multiplier :distance-from-colemak)
           (distance-from-colemak layout)))))

  ([text-statistics layout multipliers]
   (binding [multipliers multipliers]
     (rate-layout text-statistics layout))))

(deftest test-rate-layout
  (is (= 2.5708333333333333
         (rate-layout (text/text-statistics "hello" ["h" "e" "l" "o"])
                      layout/qwerty)))

  (is (= 1.1883333333333335
         (rate-layout (text/text-statistics "hello" ["h" "e" "l" "o"])
                      layout/colemak-dh)))

  (is (= 1.3166666666666667
         (rate-layout (text/text-statistics "hello" ["h" "e" "l" "o"])
                      (layout/layout-from-qwerty {"h" "k"
                                                  "e" "j"
                                                  "l" "d"
                                                  "o" "f"})))))
