(ns layouter.keyboard
  (:require
   [medley.core :as medley]))


(def keyboard-keys [{:cocoa-key-code 0  :java-key-code 65       :finger 0 :class :home       :row 1 :column 0  :qwerty-character "a" :disabled? false}
                    {:cocoa-key-code 1  :java-key-code 83       :finger 1 :class :home       :row 1 :column 1  :qwerty-character "s" :disabled? false}
                    {:cocoa-key-code 2  :java-key-code 68       :finger 2 :class :home       :row 1 :column 2  :qwerty-character "d" :disabled? false}
                    {:cocoa-key-code 3  :java-key-code 70       :finger 3 :class :home       :row 1 :column 3  :qwerty-character "f" :disabled? false}
                    {:cocoa-key-code 4  :java-key-code 72       :finger 4 :class :regular    :row 1 :column 5  :qwerty-character "h" :disabled? false}
                    {:cocoa-key-code 5  :java-key-code 71       :finger 3 :class :regular    :row 1 :column 4  :qwerty-character "g" :disabled? false}
                    {:cocoa-key-code 6  :java-key-code 90       :finger 1 :class :regular    :row 2 :column 1  :qwerty-character "z" :disabled? false}
                    {:cocoa-key-code 7  :java-key-code 88       :finger 2 :class :regular    :row 2 :column 2  :qwerty-character "x" :disabled? false}
                    {:cocoa-key-code 8  :java-key-code 67       :finger 3 :class :easy-index :row 2 :column 3  :qwerty-character "c" :disabled? false}
                    {:cocoa-key-code 9  :java-key-code 86       :finger 3 :class :sideways   :row 2 :column 4  :qwerty-character "v" :disabled? false}
                    {:cocoa-key-code 11 :java-key-code 66       :finger 3 :class :middle     :row 2 :column 4  :qwerty-character "b" :disabled? false}
                    {:cocoa-key-code 12 :java-key-code 81       :finger 0 :class :regular    :row 0 :column 0  :qwerty-character "q" :disabled? false}
                    {:cocoa-key-code 13 :java-key-code 87       :finger 1 :class :sideways   :row 0 :column 0  :qwerty-character "w" :disabled? false}
                    {:cocoa-key-code 14 :java-key-code 69       :finger 1 :class :regular    :row 0 :column 1  :qwerty-character "e" :disabled? false}
                    {:cocoa-key-code 15 :java-key-code 82       :finger 2 :class :regular    :row 0 :column 2  :qwerty-character "r" :disabled? false}
                    {:cocoa-key-code 16 :java-key-code 89       :finger 4 :class :middle     :row 0 :column 5  :qwerty-character "y" :disabled? false}
                    {:cocoa-key-code 17 :java-key-code 84       :finger 3 :class :regular    :row 0 :column 3  :qwerty-character "t" :disabled? false}
                    {:cocoa-key-code 31 :java-key-code 79       :finger 6 :class :regular    :row 0 :column 8  :qwerty-character "o" :disabled? false}
                    {:cocoa-key-code 32 :java-key-code 85       :finger 4 :class :regular    :row 0 :column 6  :qwerty-character "u" :disabled? false}
                    {:cocoa-key-code 33 :java-key-code 91       :finger 7 :class :regular    :row 0 :column 10 :qwerty-character "å" :disabled? false}
                    {:cocoa-key-code 34 :java-key-code 73       :finger 5 :class :regular    :row 0 :column 7  :qwerty-character "i" :disabled? false}
                    {:cocoa-key-code 35 :java-key-code 80       :finger 7 :class :regular    :row 0 :column 9  :qwerty-character "p" :disabled? false}
                    {:cocoa-key-code 37 :java-key-code 76       :finger 6 :class :home       :row 1 :column 8  :qwerty-character "l" :disabled? false}
                    {:cocoa-key-code 38 :java-key-code 74       :finger 4 :class :home       :row 1 :column 6  :qwerty-character "j" :disabled? false}
                    {:cocoa-key-code 39 :java-key-code 222      :finger 7 :class :regular    :row 1 :column 10 :qwerty-character "ä" :disabled? false}
                    {:cocoa-key-code 40 :java-key-code 75       :finger 5 :class :home       :row 1 :column 7  :qwerty-character "k" :disabled? false}
                    {:cocoa-key-code 41 :java-key-code 59       :finger 7 :class :home       :row 1 :column 9  :qwerty-character "ö" :disabled? false}
                    {:cocoa-key-code 43 :java-key-code 44       :finger 5 :class :regular    :row 2 :column 7  :qwerty-character "," :disabled? true}
                    {:cocoa-key-code 44 :java-key-code 47       :finger 7 :class :regular    :row 2 :column 9  :qwerty-character "-" :disabled? true}
                    {:cocoa-key-code 45 :java-key-code 78       :finger 3 :class :sideways   :row 2 :column 5  :qwerty-character "n" :disabled? false}
                    {:cocoa-key-code 46 :java-key-code 77       :finger 4 :class :easy-index :row 2 :column 6  :qwerty-character "m" :disabled? false}
                    {:cocoa-key-code 47 :java-key-code 46       :finger 6 :class :regular    :row 2 :column 8  :qwerty-character "." :disabled? true}
                    {:cocoa-key-code 50 :java-key-code 192      :finger 0 :class :regular    :row 2 :column 0  :qwerty-character "<" :disabled? true}])


(def cocoa-key-code-to-key (medley/index-by :cocoa-key-code keyboard-keys))

(def qwerty-character-to-key (medley/index-by :qwerty-character keyboard-keys))

(def java-key-code-to-cocoa-key-code (medley/map-vals :cocoa-key-code
                                                      (medley/index-by :java-key-code keyboard-keys)))

(def cocoa-key-code-to-java-key-code (medley/map-vals :java-key-code
                                                      (medley/index-by :cocoa-key-code keyboard-keys)))
