(ns advent.six
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn get-coords [line]
  (let [s (->> line
               (re-find #"(\d+), (\d+)")
               rest
               (map parse-int))]
    [(first s) (first (next s))]))

(def input (->> "coords.txt" io/resource io/reader line-seq (map get-coords)))

(defn closest-location
  [all-locs loc]
  (let [[_ coords] (reduce (fn [v c]
                             (let [distance (manhattan-distance loc c)]
                               (if (empty? v)
                                 [distance #{c}]
                                 (cond
                                   (= distance (first v)) (update v 1 conj c)
                                   (< distance (first v)) [distance #{c}]
                                   :else v))))
                           []
                           all-locs)]
    (when (= (count coords) 1) (first coords))))

(def infinite-pts
  (into #{}
        (reduce concat
                (for [y (range 44 355)]
                  (for [x [42 350]]
                    (closest-location real-input [x y]))))))


(defn unbounded-coords
  [input [[min-x min-y] [max-x max-y]]]
  (into #{}
        (remove nil?
                (concat
                 (mapcat #(vec [(closest-location input [min-x %])
                                (closest-location input [max-x %])]) (range min-y (+ max-y 1)))

                 (mapcat #(vec [(closest-location input [% min-y])
                                (closest-location input [% max-y])]) (range min-x (+ max-x 1)))))))

(defn determine-bounding-box
  [input]
  [[(first (apply min-key first input)) (last (apply min-key last input))]
   [(first (apply max-key first input)) (last (apply max-key last input))]])

(defn- coords-in-box
  [[[min-x min-y] [max-x max-y]]]
  (reduce concat
          (for [y (range min-y (+ max-y 1))]
            (for [x (range min-x (+ max-x 1))]
              [x y]))))

(defn- increment-region-for-closest-point
  [input m loc]
  (let [closest (closest-location input loc)]
    (if (nil? closest)
      m
      (assoc m closest (inc (get m closest 0))))))

(def get-bounding-box (memoize #(determine-bounding-box input)))

(defn region-size
  [input]
  (let [bbox (get-bounding-box)
        unbounded (unbounded-coords input bbox)
        [[min-x min-y] [max-x max-y]] bbox
        all-coords (coords-in-box bbox)]
    (->> (reduce (partial increment-region-for-closest-point input) {} all-coords)
         (filter #(not (contains? unbounded (first %))))
         (apply max-key last))))

(defn solve-1 []
  (region-size real-input))

(defn solve-2 []
  [input]
  (->> (coords-in-box (get-bounding-box))
       (pmap #(reduce + (map (partial manhattan-distance %) input)))
       (filter #(> 10000 %))
       count))
