(ns aoc2023.day09
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-seq #"-?\d+" %))
       (map #(mapv parse-long %))))

(defn diff [v]
  (map (fn [[a b]] (- b a)) (partition 2 1 v)))

(defn extrapolate [v]
  (->> (iterate diff v)
       (take-while #(not (every? zero? %)))
       (map last)
       (reduce + 0)))

(defn solve-part1 [input]
  (apply + (map extrapolate1 input)))

(defn solve-part2 [input]
  (apply + (map (comp extrapolate reverse) input)))
