(ns advent.one
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def frequency-input-file "frequency_input.txt")

(defn get-frequencies
  [file-name]
  (->> file-name
       io/resource
       slurp
       str/split-lines
       (map read-string)))

(defn total-frequencies
  [frequencies]
  (reduce + frequencies))

(defn first-repeated-frequency
  [frequencies]
  (loop [input (cycle frequencies)
         frequency 0
         seen #{}]
    (let [frequency (+ frequency (first input))]
      (if (contains? seen frequency)
        frequency
        (recur (rest input) frequency (conj seen frequency))))))

(defn goal-one
  []
  (total-frequencies (get-frequencies frequency-input-file)))

(defn goal-two
  []
  (first-repeated-frequency (get-frequencies frequency-input-file)))
