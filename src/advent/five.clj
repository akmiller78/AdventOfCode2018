(ns advent.five
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def raw (-> "polymer.txt" io/resource slurp str/trim))
(def data (map (comp int char) raw))

(defn opposite-polarity? [b1 b2]
  (if (or (nil? b1) (nil? b2))
    false
    (and (not= b1 b2)
         (= (bit-clear b1 5) (bit-clear b2 5)))))

(defn react-polymer
  ([input] (react-polymer input nil))
  ([input exclusions]
   (reduce #(let [last-unit (peek %1)]
              (if (and (not (empty? exclusions)) (contains? exclusions %2))
                %1
                (if (opposite-polarity? last-unit %2)
                  (pop %1)
                  (conj %1 %2)))) [] input)))

(defn solve-1 []
  (count (react-polymer data)))

(defn solve-2 []
  (reduce (fn [cur-min drop-unit]
            (let [exclusions #{drop-unit (bit-flip drop-unit 5)}
                  cnt (count (react-polymer data exclusions))]
              (if (nil? cur-min)
                cnt
                (min cur-min cnt))))
          nil
          (range 65 91)))

(time (solve-1))
(time (solve-2))
