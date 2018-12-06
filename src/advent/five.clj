(ns advent.five
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def raw (-> "polymer.txt" io/resource slurp str/trim))
(def data (map int raw))

(defn opposite-polarity? [b1 b2]
  (if (nil? b1)
    false
    (and (not= b1 b2)
         (== (bit-clear b1 5) (bit-clear b2 5)))))

(defn react-polymer
  [input]
  (reduce #(let [last-unit (peek %1)]
             (if (opposite-polarity? last-unit %2)
               (pop %1)
               (conj %1 %2))) [] input))

(defn solve-1 []
  (count (react-polymer data)))

(defn solve-2 []
  (apply min (pmap (fn [char]
                    (let [input (remove #(or (= % char)
                                             (= % (+ char 32))) data)]
                      (count (react-polymer input))))
                  (range 65 91))))
