(ns advent.four
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "guard_sleep_patterns.txt" io/resource io/reader line-seq))

(defn ->time-log-entry
  [input-str]
  (let [[year month day hour minute action]
        (rest (re-find #"(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)" input-str))]
    [(read-string (str year month day hour minute))
     (Integer/parseInt hour)
     (Integer/parseInt minute)
     action]))

;; associate the guard with the log (rather not use state so investigate this later)
(defn tag-logs
  [log]
  (let [id (atom nil)]
    (map #(let [[_ next-id] (re-find #"Guard #(\d+) begins shift" (last %))
                _ (when next-id (reset! id next-id))]
            (into [@id] %))
         log)))

(def log
  (->> input
       (map ->time-log-entry)
       (sort-by first)
       tag-logs))

(def falls-asleep-or-wakes (filter #(or (= "falls asleep" (nth % 4))
                                        (= "wakes up" (nth % 4)))))

(def sleep-log (transduce falls-asleep-or-wakes conj log))

(defn most-minutes-slept
  [sleep-log]
  (->> sleep-log
       (partition 2)
       (map (fn [[sleep wake]]
              [(first sleep) (- (nth wake 3) (nth sleep 3))]))
       (group-by first)
       (map (fn [[id entries]]
              {:id id :slept (reduce + (map last entries))}))
       (sort-by :slept)
       (reverse)
       (take 1)
       first
       :id))

(defn longest-minute-slept
  [sleep-log guard]
  (->> sleep-log
       (filter #(= guard (first %)))
       (partition 2)
       (reduce (fn [minutes [sleep wake]]
                 (let [slept (range (nth sleep 3) (nth wake 3))]
                   (conj minutes slept)))
               [])
       (apply concat)
       (frequencies)
       (sort-by val)
       (reverse)
       (take 1)
       first))

(defn solve-1
  []
  (let [guard (most-minutes-slept sleep-log)
        minute (first (longest-minute-slept sleep-log guard))]
    (* (read-string guard) minute)))

(defn solve-2
  []
  (let [ids (distinct (map first sleep-log))
        info
        (->> ids
             (map #(let [minute (longest-minute-slept sleep-log %)]
                     {:guard % :minute (first minute) :time (last minute)}))
             (sort-by :time)
             (reverse)
             (take 1)
             first)]
    (* (read-string (:guard info)) (:minute info))))

(comment
  (time (solve-1))
  (time (solve-2))

  )
