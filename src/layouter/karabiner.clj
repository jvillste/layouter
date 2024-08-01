(ns layouter.karabiner
  (:require [jsonista.core :as jsonista]
            [clojure.java.io :as io]
            [camel-snake-kebab.core :as camel-snake-kebab]))

(defn read-json-string [string]
  (jsonista/read-value (io/input-stream (.getBytes string
                                                   "UTF-8"))
                       (jsonista/object-mapper {:decode-key-fn camel-snake-kebab/->kebab-case-keyword})))

(defn write-json-string [value]
  (jsonista/write-value-as-string value
                                  (jsonista/object-mapper {:encode-key-fn camel-snake-kebab/->snake_case_string
                                                           :pretty true})))

(def config-file-path "/Users/jukka/.config/karabiner/karabiner.json")

(defn layer-key [layer from to]
  {:type "basic",
   :conditions [{:type "variable_if"
                 :name layer
                 :value 1}]
   :from from,
   :to [to]})

(defn define-layer [layer-name from & mappings]
  {:description layer-name,
   :manipulators (concat [{:type "basic",
                           :from from,
                           :to-after-key-up [{:set-variable {:name layer-name, :value 0}}],
                           :to [{:set-variable {:name layer-name, :value 1}}]}]
                         (for [[from to] (partition 2 mappings)]
                           (layer-key layer-name
                                      from
                                      to)))})

(def rules [{:description "control-j to enter",
             :manipulators
             [{:type "basic",
               :from
               {:key-code "j",
                :modifiers {:mandatory ["left_control"], :optional ["any"]}},
               :to [{:key-code "return_or_enter"}]}]}

            {:description "control-ö to backspace",
             :manipulators
             [{:type "basic",
               :from
               {:key-code "semicolon",
                :modifiers {:mandatory ["left_control"], :optional ["any"]}},
               :to [{:key-code "delete_or_backspace"}]}]}

            (define-layer "layer 1"
              {:key-code "grave_accent_and_tilde"
               :modifiers {:mandatory [], :optional ["any"]}}

              {:key-code "s"}
              {:key-code "8"
               :modifiers ["left_shift"]} ;; (

              {:key-code "s"
               :modifiers {:mandatory ["left_command"]}}
              {:key-code "9"
               :modifiers ["left_shift"]} ;; )

              {:key-code "d"}
              {:key-code "8"
               :modifiers ["left_shift" "left_option"]} ;; {

              {:key-code "d"
               :modifiers {:mandatory ["left_command"]}}
              {:key-code "9"
               :modifiers ["left_shift" "left_option"]} ;; }

              {:key-code "f"}
              {:key-code "8"
               :modifiers ["left_option"]} ;; [

              {:key-code "f"
               :modifiers {:mandatory ["left_command"]}}
              {:key-code "9"
               :modifiers ["left_option"]} ;; ]

              {:key-code "c"}
              {:key-code "grave_accent_and_tilde"} ;; [

              {:key-code "c" :modifiers {:mandatory ["left_command"]}}
              {:key-code "grave_accent_and_tilde" :modifiers ["left_shift"]} ;; >

              {:key-code "z"}
              {:key-code "1" :modifiers ["left_shift"]} ;; !

              {:key-code "x"}
              {:key-code "2" :modifiers ["left_shift"]} ;; "

              {:key-code "z" :modifiers {:mandatory ["left_command"]}} ;; TODO: x does not work here?
              {:key-code "2" :modifiers ["left_option"]} ;; @

              {:key-code "r"}
              {:key-code "3" :modifiers ["left_shift"]} ;; #

              {:key-code "t"}
              {:key-code "4" :modifiers ["left_shift"]} ;; €

              {:key-code "t" :modifiers {:mandatory ["left_command"]}}
              {:key-code "4" :modifiers ["left_option"]} ;; $

              {:key-code "y"}
              {:key-code "5" :modifiers ["left_shift"]} ;; %

              {:key-code "h"}
              {:key-code "6" :modifiers ["left_shift"]} ;; &

              {:key-code "n"}
              {:key-code "7" :modifiers ["left_shift"]} ;; /

              {:key-code "n" :modifiers {:mandatory ["left_command"]}}
              {:key-code "7" :modifiers ["right_shift" "left_option"]} ;; \

              {:key-code "n" :modifiers {:mandatory ["right_command"]}}
              {:key-code "7" :modifiers ["left_option"]} ;; |

              {:key-code "p"}
              {:key-code "0" :modifiers ["left_shift"]} ;; =

              {:key-code "p" :modifiers {:mandatory ["left_command"]}}
              {:key-code "equal_sign" :modifiers ["left_shift"]} ;; `

              {:key-code "slash"}
              {:key-code "hyphen" :modifiers ["left_shift"]} ;; ?

              {:key-code "slash" :modifiers {:mandatory ["left_command"]}}
              {:key-code "hyphen"} ;; +

              {:key-code "u"}
              {:key-code "1"}

              {:key-code "i"}
              {:key-code "2"}

              {:key-code "o"}
              {:key-code "3"}

              {:key-code "j"}
              {:key-code "4"}

              {:key-code "k"}
              {:key-code "5"}

              {:key-code "l"}
              {:key-code "6"}

              {:key-code "m"}
              {:key-code "7"}

              {:key-code "comma"}
              {:key-code "8"}

              {:key-code "period"}
              {:key-code "9"}

              {:key-code "semicolon"}
              {:key-code "0"})
            ])

(defn update-config-file []
  (spit config-file-path
        (-> (slurp config-file-path)
            (read-json-string)
            (assoc-in [:profiles
                       0
                       :complex-modifications
                       :rules]
                      rules)
            (write-json-string))))

(comment
  (update-config-file)

  (read-json-string (slurp "temp/rule.json")))
