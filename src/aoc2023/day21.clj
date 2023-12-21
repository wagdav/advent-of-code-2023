(ns aoc2023.day21
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        mid (/ (count lines) 2)]
    (assert (= (count lines) (count (first lines))))
    {:size (count lines)
     :garden (into #{}
              (for [[row line] (map-indexed vector lines)
                    [col c] (map-indexed vector line)
                    :when (#{\. \S} c)]
                [row col]))
     :start (first (for [[row line] (map-indexed vector lines)
                         [col c] (map-indexed vector line)
                         :when (= c \S)]
                     [row col]))}))

(defn exercise [garden start steps]
  (loop [remaining steps, frontier #{start}]
    (let [new-frontier (set (for [dir [[0 1] [0 -1] [-1 0] [1 0]]
                                  p frontier
                                  :let [f (mapv + p dir)]
                                  :when (and (garden f))]
                              f))]
      (if (or (zero? remaining) (empty? new-frontier))
        (count frontier)
        (recur (dec remaining) new-frontier)))))

; naive
(defn exercise-infinite [garden size start steps]
  (loop [res [], i 0, frontier #{start}]
    (let [new-frontier (set (for [dir [[0 1] [0 -1] [-1 0] [1 0]]
                                  p frontier
                                  :let [[r c :as f] (mapv + p dir)
                                        fm (mapv #(mod % size) f)]
                                  :when (garden fm)]
                              f))]
      (if (> i (apply max steps))
        res
        (recur (if (steps i)
                 (conj res (count frontier))
                 res)
               (inc i) new-frontier)))))

(defn solve-part1 [{:keys [garden start]}]
  (exercise garden start 64))

(defn lagrange
  [y0 y1 y2]
  [(+ (/ y0 2) (- y1) (/ y2 2))
   (+ (* -3 (/ y0 2)) (* 2 y1) (- (/ y2 2)))
   y0])

(defn solve-part2 [{:keys [garden size start]}]
  (let [mid (long (math/floor (/ size 2)))
        [y0 y1 y2] (exercise-infinite garden size start #{mid (+ mid size) (+ mid size size)})
        [x0 x1 x2] (lagrange y0 y1 y2)
        target (/ (- 26501365 mid) size)]
    (+ (* x0 target target)
       (* x1 target)
       x2)))
