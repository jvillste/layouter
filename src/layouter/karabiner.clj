(ns layouter.karabiner
  (:require [jsonista.core :as jsonista]
            [clojure.java.io :as io]
            [camel-snake-kebab.core :as camel-snake-kebab]
            [clojure.test :refer [deftest is]]))

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
   :to (if (list? to)
         (vec to)
         [to])})

(defn expand-vectors [values]
  (loop [expanded-values []
         remaining-values values]
    (if-let [value (first remaining-values)]
      (if (vector? value)
        (recur (concat expanded-values
                       value)
               (rest remaining-values))
        (recur (concat expanded-values
                       [value])
               (rest remaining-values)))
      expanded-values)))

(deftest test-expand-vectors
  (is (= '(1 2 3 4)
         (expand-vectors [1 [2 3] 4]))))

(defn define-layer [layer-name from & mappings]
  {:description layer-name,
   :manipulators (concat [{:type "basic",
                           :from from,
                           :to-after-key-up [{:set-variable {:name layer-name, :value 0}}],
                           :to [{:set-variable {:name layer-name, :value 1}}]}]
                         (for [[from to] (partition 2 (expand-vectors mappings))]
                           (layer-key layer-name
                                      from
                                      to)))})

(defn shell-command [modifiers key-code shell-command]
  {:description shell-command,
   :manipulators
   [{:type "basic",
     :from
     {:key-code key-code,
      :modifiers {:mandatory modifiers, :optional []}},
     :to [{:shell-command shell-command}]}]})

(defn workspace-key-code [workspace-number]
  (str (mod workspace-number 10)))

(deftest test-workspace-key-code
  (is (= "1"
         (workspace-key-code 1)))

  (is (= "0"
         (workspace-key-code 10))))

(defn workspace-modifiers [workspace-number]
  (if (>= 10 workspace-number)
    ["left_control"]
    ["left_shift" "left_control"]))

(defn workspace [workspace-number modifier key]
  [{:key-code key :modifiers {:mandatory [modifier]}}
   {:key-code (workspace-key-code workspace-number) :modifiers (workspace-modifiers workspace-number)}
   {:key-code key :modifiers {:mandatory ["left_shift" modifier]}}
   {:shell-command (str "/opt/homebrew/bin/yabai -m window --space " workspace-number)}])

(def rules [{:description "control-m to enter",
             :manipulators
             [{:type "basic",
               :from
               {:key-code "m",
                :modifiers {:mandatory ["left_control"], :optional ["any"]}},
               :to [{:key-code "return_or_enter"}]}]}

            {:description "control-ö to backspace",
             :manipulators
             [{:type "basic",
               :from
               {:key-code "semicolon",
                :modifiers {:mandatory ["left_control"], :optional ["any"]}},
               :to [{:key-code "delete_or_backspace"}]}]}

            {:description "caps lock to control",
             :manipulators
             [{:type "basic",
               :from
               {:key-code "caps_lock",
                :modifiers {:optional ["any"]}},
               :to [{:key-code "left_control"}]}]}

            {:description "left command e to caps lock",
             :manipulators
             [{:type "basic",
               :from
               {:key-code "e",
                :modifiers {:mandatory ["left_command"] :optional ["caps_lock"]}},
               :to [{:key-code "caps_lock"}]}]}

            (define-layer "layer 1"
              {:key-code "grave_accent_and_tilde" :modifiers {:mandatory [], :optional ["any"]}}

              {:key-code "g"}
              {:key-code "8" :modifiers ["left_shift" "left_option"]} ;; {

              {:key-code "g" :modifiers {:mandatory ["left_command"]}}
              {:key-code "9" :modifiers ["left_shift" "left_option"]} ;; }

              {:key-code "d"}
              {:key-code "8" :modifiers ["left_option"]} ;; [

              {:key-code "d" :modifiers {:mandatory ["left_command"]}}
              {:key-code "9" :modifiers ["left_option"]} ;; ]

              {:key-code "f"}
              {:key-code "8" :modifiers ["left_shift"]} ;; (

              {:key-code "f" :modifiers {:mandatory ["left_command"]}}
              {:key-code "9" :modifiers ["left_shift"]} ;; )

              {:key-code "c"}
              {:key-code "non_us_backslash"} ;; <

              {:key-code "c" :modifiers {:mandatory ["left_command"]}}
              {:key-code "non_us_backslash" :modifiers ["left_shift"]} ;; >

              {:key-code "z"}
              {:key-code "1" :modifiers ["left_shift"]} ;; !

              {:key-code "x"}
              {:key-code "2" :modifiers ["left_shift"]} ;; "

              {:key-code "e"}
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

              {:key-code "p" :modifiers {:mandatory ["right_command"]}}
              {:key-code "backslash"} ;; '

              {:key-code "open_bracket"}
              {:key-code "backslash" :modifiers ["left_shift"]} ;; *

              {:key-code "open_bracket" :modifiers {:mandatory ["left_command"]}}
              (list {:key-code "close_bracket" :modifiers ["left_option"]} ;; ~
                    {:key-code "spacebar"})

              {:key-code "slash"}
              {:key-code "hyphen" :modifiers ["left_shift"]} ;; ?

              {:key-code "slash" :modifiers {:mandatory ["left_command"] :optional ["any"]}}
              {:key-code "hyphen"} ;; +

              {:key-code "m"}
              {:key-code "1"}

              {:key-code "comma"}
              {:key-code "2"}

              {:key-code "period"}
              {:key-code "3"}

              {:key-code "j"}
              {:key-code "4"}

              {:key-code "k"}
              {:key-code "5"}

              {:key-code "l"}
              {:key-code "6"}

              {:key-code "u"}
              {:key-code "7"}

              {:key-code "i"}
              {:key-code "8"}

              {:key-code "o"}
              {:key-code "9"}


              {:key-code "semicolon"}
              {:key-code "0"}


              ;; navigation

              {:key-code "j" :modifiers {:mandatory ["left_command"]}}
              {:key-code "left_arrow"}

              {:key-code "k" :modifiers {:mandatory ["left_command"]}}
              {:key-code "down_arrow"}

              {:key-code "l" :modifiers {:mandatory ["left_command"]}}
              {:key-code "right_arrow"}

              {:key-code "semicolon" :modifiers {:mandatory ["left_command"]}}
              {:key-code "up_arrow"}

              {:key-code "v"}
              {:key-code "down_arrow"  :modifiers ["left_option"]} ;; page down

              {:key-code "v" :modifiers {:mandatory ["left_command"]}}
              {:key-code "up_arrow"  :modifiers ["left_option"]} ;; page up

              {:key-code "g"}
              {:key-code "escape"}


              ;; applications

              {:key-code "g" :modifiers {:mandatory ["right_command"]}}
              {:shell-command "open '/Applications/Google Chrome.app'"}

              {:key-code "t" :modifiers {:mandatory ["right_command"]}}
              {:shell-command "open '/System/Applications/Utilities/Terminal.app'"}

              {:key-code "f" :modifiers {:mandatory ["right_command"]}}
              {:shell-command "open '/System/Library/CoreServices/Finder.app'"}

              {:key-code "e" :modifiers {:mandatory ["right_command"]}}
              {:shell-command "open -n /Applications/Emacs.app"}

              ;; workspaces

              (workspace 1 "right_command" "m")
              (workspace 2 "right_command" "comma")
              (workspace 3 "right_command" "period")
              (workspace 4 "right_command" "j")
              (workspace 5 "right_command" "k")
              (workspace 6 "right_command" "l")
              (workspace 7 "right_command" "u")
              (workspace 8 "right_command" "i")
              (workspace 9 "right_command" "o")
              (workspace 10 "right_command" "semicolon")
              (workspace 11 "left_command" "m")
              (workspace 12 "left_command" "comma")
              (workspace 13 "left_command" "period")
              (workspace 14 "left_command" "u")
              (workspace 15 "left_command" "i")
              (workspace 16 "left_command" "o"))

            (shell-command ["right_command"] "c" "zsh --login -c \"jenv exec /Users/jukka/bin/utils convert-clipboard-to-plain-text\"")
            (shell-command ["right_command" "left_shift"] "c" "zsh --login -c \"jenv exec /Users/jukka/bin/utils clean-up-jira-issue-title-in-clipboard\"")
            (shell-command ["left_command" "left_shift"] "c" "zsh --login -c \"jenv exec /Users/jukka/bin/utils jira-issue-title-to-branch-name-in-clipboard\"")

            (shell-command ["right_command"] "a" "/opt/homebrew/bin/yabai -m space --layout bsp")
            (shell-command ["right_command"] "s" "/opt/homebrew/bin/yabai -m space --layout stack")
            (shell-command ["right_command"] "q" "/opt/homebrew/bin/yabai -m space --layout float")

            (shell-command ["right_command"] "d" "/opt/homebrew/bin/yabai -m window --toggle split")
            (shell-command ["right_command"] "r" "/opt/homebrew/bin/yabai -m window --toggle float")
            (shell-command ["right_command"] "u" "/opt/homebrew/bin/yabai -m space --rotate 90")
            (shell-command ["right_command"] "y" "/opt/homebrew/bin/yabai -m space --mirror x-axis")
            (shell-command ["right_command"] "h" "/opt/homebrew/bin/yabai -m space --mirror y-axis")
            (shell-command ["right_command"] "l" "/opt/homebrew/bin/yabai -m space --balance")
            (shell-command ["right_command"] "j" "/opt/homebrew/bin/yabai -m window --focus next || /opt/homebrew/bin/yabai -m window --focus first")
            (shell-command ["right_command"] "k" "/opt/homebrew/bin/yabai -m window --focus prev || /opt/homebrew/bin/yabai -m window --focus last")
            (shell-command ["right_command"] "n" "/opt/homebrew/bin/yabai -m window --swap next || /opt/homebrew/bin/yabai -m window --swap first")
            (shell-command ["right_command"] "m" "/opt/homebrew/bin/yabai -m window --swap prev || /opt/homebrew/bin/yabai -m window --swap last")
            (shell-command ["right_command"] "o" "/opt/homebrew/bin/yabai -m window --insert north")
            (shell-command ["right_command"] "z" "/opt/homebrew/bin/displayplacer \"id:1C794DBC-DA89-41CD-B7DB-71583EBC644E degree:0\"")
            (shell-command ["right_command"] "x" "/opt/homebrew/bin/displayplacer \"id:1C794DBC-DA89-41CD-B7DB-71583EBC644E degree:90\"")
            (shell-command ["right_command" "left_shift"] "k" "/opt/homebrew/bin/yabai -m window --focus stack.next || /opt/homebrew/bin/yabai -m window --focus stack.first")
            (shell-command ["right_command" "left_shift"] "j" "/opt/homebrew/bin/yabai -m window --focus stack.prev || /opt/homebrew/bin/yabai -m window --focus stack.last")

            (shell-command ["right_command" "left_shift"] "p" "zsh --login -c \"pwcopy copy-password-from-keychain-to-clipboard p\"")
            (shell-command ["right_command" "left_shift"] "i" "zsh --login -c \"pwcopy copy-password-from-keychain-to-clipboard d\"")
            (shell-command ["right_command" "left_shift"] "o" "zsh --login -c \"pwcopy copy-password-from-keychain-to-clipboard di\"")
            (shell-command ["right_command" "left_shift"] "d" "zsh --login -c \"pwcopy copy-password-from-keychain-to-clipboard de\"")

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
