(ns layouter.demo
  (:require
   [layouter.gui :as gui]
   [layouter.key-log :as key-log]
   [layouter.keyboard-view :as keyboard-view]
   [layouter.layout :as layout]
   [layouter.layout-comparison-view :as layout-comparison-view]
   [layouter.optimize :as optimize]
   [layouter.rating :as rating]
   [layouter.text :as text]
   [layouter.view :as view]))
































(comment

  layout/qwerty

  (view/start-view (fn [] (->> (:layout layout/oeita)
                               layout/layout-to-cocoa-key-code-to-character
                               keyboard-view/keyboard-view
                               gui/black-background)))

  text/finnish-statistics-without-å
  key-log/key-log-text-statistics

  ;; see keylogger.c

  )






































(def emphasize-roll-key-and-vertical-movement-multipliers {:digram-roll 1,
                                                           :trigram-roll 0,
                                                           :key-rating 1,
                                                           :finger-type 0.1,
                                                           :horizontal-movement 0.1,
                                                           :vertical-movement 1,
                                                           :hand-balance 0.1
                                                           :dist-from-colemak 0.1})


(comment


  (rating/rate-key-pair (map (layout/layout-to-character-to-key layout/qwerty)
                             ["k" "j"]))


  (rating/rate-key-pair (map (layout/layout-to-character-to-key layout/qwerty)
                             ["m" "y"]))



  (rating/rate-key ((layout/layout-to-character-to-key layout/qwerty) "j"))

  (rating/rate-key ((layout/layout-to-character-to-key layout/qwerty) "q"))



  (rating/rate-layout text/finnish-statistics-without-å
                      layout/qwerty
                      emphasize-roll-key-and-vertical-movement-multipliers)

  (rating/rate-layout text/finnish-statistics-without-å
                      layout/colemak-dh
                      emphasize-roll-key-and-vertical-movement-multipliers)

  (rating/rate-layout text/english-statistics
                      layout/colemak-dh
                      emphasize-roll-key-and-vertical-movement-multipliers)

  (repeatedly 5 #(rating/rate-layout text/finnish-statistics-without-å
                                     (optimize/random-layout)
                                     emphasize-roll-key-and-vertical-movement-multipliers))

  )




























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

(comment
  (do (reset! optimize/layout-optimization-log-atom [])
      (reset! optimize/optimization-history-atom [])
      (view/refresh-view!))

  (view/start-view #'layout-comparison-view/optimized-layouts-comparison-view-2
                   {:join? false})

  (doto (Thread. (fn []
                   (reset! optimize/stop-requested?-atom false)
                   (optimize/optimize-repeatedly! (fn []
                                                    (optimize/optimize-layout static-metaparameters
                                                                              emphasize-roll-key-and-vertical-movement-multipliers
                                                                              text/finnish-statistics-without-å
                                                                              "temp/layout-optimization-demo-log.edn"
                                                                              {:maximum-number-of-generations-without-improvement 300})))))
    (.setName "repeating layout optimization")
    (.start))

  (reset! optimize/stop-requested?-atom true)


  ;; Try this:
  ;; use map instead of pmap for rating

  )
