(ns aoc2023.day06
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn transpose [v]
  (apply mapv vector v))

(defn parse-input [input]
  (str/split-lines input))

(defn wins-naive [[race-time record]]
   (count (->> (range race-time)
               (map #(* % (- race-time %)))
               (filter #(> % record)))))

(defn wins-fast
  [[race-time record]]
  ; The distance convered is d = wait * (race-time - wait).  This is quadradic
  ; in wait.  Solve for wait and compute the difference between the roots.
  (let [d (math/sqrt (- (math/pow race-time 2) (* 4 record)))
        w1 (/ (+ race-time d) 2)
        w2 (/ (- race-time d) 2)]
    (long (dec (- (math/ceil w1) (math/floor w2))))))

(defn solve-part1 [input]
  (let [races (->> input
                   (map #(re-seq #"-?\d+" %))
                   (map #(map parse-long %))
                   transpose)]
    (apply * (map wins-fast races))))

(defn solve-part2 [input]
  (let [big-race (->> input
                      (map #(re-seq #"-?\d+" %))
                      (map str/join)
                      (map parse-long))]
    (wins-fast big-race)))
