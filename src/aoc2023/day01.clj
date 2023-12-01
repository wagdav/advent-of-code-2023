(ns aoc2023.day01
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(defn line->calibration1 [line]
  (->> (re-seq #"\d" line)
       ((juxt first last))
       (apply str)
       (parse-long)))

(def digits {"one" "1", "two" "2", "three" "3", "four" "4", "five" "5", "six" "6", "seven" "7", "eight" "8", "nine" "9"})

(defn line->calibration2 [line]
  (let [f (->> (str/replace line (re-pattern (str/join "|" (keys digits))) digits)
               (re-seq #"\d")
               first)
        l (->> (str/replace (str/reverse line)
                            (re-pattern (->> (keys digits) (map str/reverse) (str/join "|")))
                            (update-keys digits str/reverse))
               (re-seq #"\d")
               first)]
   (parse-long (str f l))))

(defn solve-part1 [input]
  (->> (map line->calibration1 input)
       (apply +)))

(defn solve-part2 [input]
  (->> (map line->calibration2 input)
       (apply +)))
