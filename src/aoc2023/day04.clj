(ns aoc2023.day04
  (:require [clojure.string :as str]
            [clojure.math :refer [pow]]))

(def example-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
")

(defn parse-numbers [line]
  (->> (re-seq #"-?\d+" line)
       (mapv parse-long)))

(defn parse-line [line]
  (let [[c w d] (str/split line #":|\|")]
    [(dec (first (parse-numbers c))) (set (parse-numbers w)) (parse-numbers d)]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv parse-line)))

(defn matches [[_ winning drawn]]
  (count (filter winning drawn)))

(defn score [card]
  (let [m (matches card)]
    (if (zero? m)
      0
      (long (pow 2 (dec m))))))

(defn win-copies [[id _ _ :as card]]
  (range (inc id) (+ 1 id (matches card))))

(defn solve-part1 [input]
  (apply + (map score input)))

(defn process [cards total won]
  (if (empty? won)
    total
    (recur
      cards
      (merge-with + total won)
      (apply merge-with +
        (for [[id qty] won]
          (update-vals
            (frequencies (win-copies (cards id)))
            #(* qty %)))))))

(defn solve-part2 [input]
  (apply +
   (vals (process input {} (zipmap (range (count input)) (repeat 1))))))
