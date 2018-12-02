(ns advent.two
  (:require [clojure.java.io :as io]))

(defn- get-boxids
  [file-name]
  (->> file-name
       io/resource
       io/reader
       line-seq))

(defn get-checksum
  [ids]
  (let [[twos threes]
        (reduce
         (fn [counts id]
           (let [vals (->> (frequencies id)
                           vals
                           distinct)
                 twos (count (filter #(= % 2) vals))
                 threes (count (filter #(= % 3) vals))]
             [(+ (first counts) twos)
              (+ (last counts) threes)]))
         [0 0]
         ids)]
    (* twos threes)))

(defn- close-match [s1 s2]
  (if (= (count (filter #(= % false)
                        (map (comp zero? compare)
                             (seq s1)
                             (seq s2))))
         1)
    [s1 s2]
    nil))

(defn find-most-similar-ids
  [ids]
  (let [[id1 id2]
        (loop [ids ids]
          (let [[cur & remaining] ids
                matched (some #(close-match cur %) ids)]
            (if matched
              matched
              (recur remaining))))]
    {:id1 id1
     :id2 id2
     :common
     (->> (map #(when (= %1 %2)
                  %1) id1 id2)
          (filter some?)
          (apply str))}))
