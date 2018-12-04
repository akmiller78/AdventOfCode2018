(ns advent.four
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "guard_sleep_patterns.txt" io/resource io/reader line-seq))

(defn str->int [s]
  (Integer/parseInt s))

(defn ->time-log-entry
  "Returns log entry as vector of 3 items:
  timestamp (as string), minute, action"
  [input-str]
  (let [[year month day hour minute action]
        (rest (re-find #"(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)" input-str))]
    [(str year month day hour minute)
     (str->int minute)
     action]))

(defn assoc-guard
  "Helper function to assign guard to all log entries"
  [log]
  (last
   (reduce (fn [[id guard-log] entry]
             (let [id (or (last (re-find #"Guard #(\d+) begins shift" (last entry))) id)]
               [id (conj guard-log (into [id] entry))]))
           [nil []]
           log)))

(def sleep-log
  (memoize
   #(->> input
         (map ->time-log-entry)
         (sort-by first)
         (assoc-guard)
         (filter (fn [entry]
                   (or (= "falls asleep" (nth entry 3))
                       (= "wakes up" (nth entry 3))))))))

(defn most-minutes-slept
  [sleep-log]
  (->> sleep-log
       (partition 2)
       (map (fn [[sleep wake]]
              [(first sleep) (- (nth wake 2) (nth sleep 2))]))
       (group-by first)
       (map (fn [[id entries]]
              {:id id :slept (reduce + (map last entries))}))
       (apply max-key :slept)
       :id))

(defn longest-minute-slept
  [sleep-log guard]
  (->> sleep-log
       (filter #(= guard (first %)))
       (partition 2)
       (reduce (fn [minutes [sleep wake]]
                 (let [slept (range (nth sleep 2) (nth wake 2))]
                   (conj minutes slept)))
               [])
       (apply concat)
       (frequencies)
       (apply max-key val)))

(defn solve-1
  []
  (let [guard (most-minutes-slept (sleep-log))
        minute (first (longest-minute-slept (sleep-log) guard))]
    (* (str->int guard) minute)))

(defn solve-2
  []
  (let [ids (distinct (map first (sleep-log)))
        info
        (->> ids
             (map #(let [minute (longest-minute-slept (sleep-log) %)]
                     {:guard % :minute (first minute) :time (last minute)}))
             (sort-by :time)
             (reverse)
             (take 1)
             first)]
    (* (str->int (:guard info)) (:minute info))))

(comment

  (time (solve-1))
  (time (solve-2))

  )
