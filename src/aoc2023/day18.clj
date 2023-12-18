(ns aoc2023.day18
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.java.io :as io]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[d v c] (str/split line #" ")]
                [d (parse-long v) c])))))

(def directions {"L" [0 -1] "R" [0 1] "U" [-1 0] "D" [1 0]})

(defn move [pos d q]
  (let [delta (map #(* q %) (directions d))]
    (mapv + pos delta)))

(defn moves->points [moves]
  (reduce
    (fn [points [direction amount]]
      (conj points (move (last points) direction amount)))
    [[0 0]]
    moves))

; Pick's theorem:  A = i + b/2 - 1
;     A = polygon's area
;     i = number of interior points
;     b = number of boundary points
;
; The lagoon's size is (i + b), which comes from rearranging Pick's theorem:
;
;     i + b = A + b/2 + 1
;
; where A is computed using the Shoelace formula and b is the length of the
; perimeter.
(defn lagoon [points perimeter]
  (let [shoelace (apply + (for [[[r1 c1] [r2 c2]] (partition 2 1 points)]
                            (/ (- (* r1 c2) (* r2 c1)) 2)))]
     (inc (+ (abs shoelace) (/ perimeter 2)))))

(defn solve-part1 [input]
  (let [points (moves->points input)
        perimeter (apply + (map second input))]
    (lagoon points perimeter)))

(defn color->instruction [c]
  (let [amt (subs c 2 7)
        d (subs c 7 8)
        table {0 "R" 1 "D" 2 "L" 3 "U"}]
    [(table (parse-long d)) (Integer/parseInt amt 16)]))

(defn solve-part2 [input]
  (let [moves (for [[_ _ color] input] (color->instruction color))
        perimeter (apply + (map second moves))
        points (moves->points moves)]
    (lagoon points perimeter)))
