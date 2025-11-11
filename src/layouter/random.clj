(ns layouter.random
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

(defmacro with-fixed-random-seed [& body]
  `(binding [random-double (create-random-double-function 1)]
     ~@body))

(defn pick-random [collection]
  (nth collection
       (* (random-double)
          (count collection))))
