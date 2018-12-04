(ns advent.three
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set]))

(def input (->> "santa_suit_fabric_claims.txt" io/resource io/reader line-seq))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\d+" s)))

(defn location-1d
  [width [x y]]
  (+ x (* width y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part One
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-claim-info
  [input]
  (->> input
       (map #(let [[claim-no x1 y1 x2 y2]
                   (->> %
                        (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                        rest
                        (map parse-int))]
               {:claim claim-no
                :coords [[x1 y1] [(+ x1 x2) (+ y1 y2)]]}))))

(defn get-fabric
  [size]
  (vec (replicate size 0)))

(defn- mark-row
  [fabric start-idx end-idx]
  (reduce
   #(update %1 %2 inc)
   fabric
   (range start-idx end-idx)))

(defn- mark-fabric
  [fabric width [[x1 y1][x2 y2]]]
  (let [start-row y1
        end-row y2]
    (reduce
     (fn [f row]
       (let [start-idx (location-1d width [x1 row])
             end-idx (location-1d width [x2 row])]
         (mark-row f start-idx end-idx)))
     fabric
     (range start-row end-row))))

(defn get-claimed
  [input board-width]
  (let [size (* board-width board-width)
        fabric (get-fabric size)
        claims (get-claim-info input)]
    (reduce
     (fn [fabric claim]
       (mark-fabric fabric board-width (:coords claim)))
     fabric
     claims)))

(defn get-claimed-square-inches
  [input board-width]
  (let [claimed (get-claimed input board-width)]
    (->> claimed
         (filter #(> % 1))
         count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part Two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-indices
  [v]
  (->> v
       (keep-indexed #(when (= 1 %2) %1))
       set))

(defn- get-claim-indices
  [width claim]
  (let [[[x1 y1][x2 y2]] (:coords claim)
        start-row y1
        end-row y2]
    (reduce
     (fn [indices row]
       (let [start-idx (location-1d width [x1 row])
             end-idx (location-1d width [x2 row])]
         (apply conj indices (range start-idx end-idx))))
     #{}
     (range start-row end-row))))

(defn find-non-repeated-claims
  [input board-width]
  (let [size (* board-width board-width)
        fabric (get-fabric size)
        claims (get-claim-info input)
        single-claim-indices (get-indices
                              (get-claimed input board-width))]
    (->> claims
         (keep (fn [c]
                 (let [indices (get-claim-indices board-width c)]
                   (when (clojure.set/subset? indices single-claim-indices)
                     c)))))))

(comment

  (def test-input
    ["#1 @ 1,3: 2x1"
     "#2 @ 3,1: 2x2"
     "#3 @ 6,6: 2x2"
     "#4 @ 1,3: 2x1"
     "#5 @ 0,0: 4x3"])

  (def c (second (get-claim-info test-input)))

  (get-claim-indices 8 c)

  (get-claimed-square-inches test-input 8)

  (time (get-claimed-square-inches input 1000))

  (time (find-non-repeated-claims input 1000))

  )
