(ns aoc2023.day11
  (:require [clojure.string :as str]))

(defn empty-coords [rows]
  (for [[i row] (map-indexed vector rows)
        :when (= (set row) #{\.})]
    i))

(defn transpose [v]
  (apply mapv vector v))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:galaxies
     (for [[i row] (map-indexed vector lines)
           [j c]  (map-indexed vector row)
           :when (= c \#)]
       [i j])
     :empty-rows (empty-coords lines)
     :empty-cols (empty-coords (transpose lines))}))

(defn expand-rows [galaxies empty-rows factor]
  (loop [gs galaxies, rs empty-rows]
    (if (seq rs)
      (recur
        (map
          (fn [[row col]] (if (< row (first rs))
                            [row col]
                            [(+ row (dec factor)) col]))
          gs)
        (map #(+ % (dec factor)) (rest rs)))
      gs)))

(defn expand-cols [galaxies empty-cols factor]
  (loop [gs galaxies, cs empty-cols]
    (if (seq cs)
      (recur
        (map
          (fn [[row col]] (if (< col (first cs))
                            [row col]
                            [row (+ col (dec factor))]))
          gs)
        (map #(+ % (dec factor)) (rest cs)))
      gs)))

(defn expand [{:keys [galaxies empty-rows empty-cols]} factor]
  (-> galaxies
      (expand-rows empty-rows factor)
      (expand-cols empty-cols factor)))

(defn manhattan [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (apply +)))

(defn distances [input factor]
  (let [gs (expand input factor)]
    (/ (apply + (for [g1 gs, g2 gs] (manhattan g1 g2))) 2)))

(defn solve-part1 [input]
  (distances input 2))

(defn solve-part2 [input]
  (distances input 1000000))
