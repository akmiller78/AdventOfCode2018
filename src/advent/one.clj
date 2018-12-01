(ns advent.one
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-file "frequency_input.txt")

(defn get-data
  [file-name]
  (->> file-name
       io/resource
       slurp
       str/split-lines
       (map read-string)))

(defn total-frequencies
  [data]
  (reduce + data))

(defn first-repeated-frequency
  [data]
  (loop [input (cycle data)
         frequency 0
         seen #{}]
    (let [frequency (+ frequency (first input))]
      (if (contains? seen frequency)
        frequency
        (recur (rest input) frequency (conj seen frequency))))))

(defn goal-one
  []
  (total-frequencies (get-data input-file)))

(defn goal-two
  []
  (first-repeated-frequency (get-data input-file)))
