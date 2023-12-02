(ns aoc2023.day02
  (:require [clojure.string :as str]))

(defn parse-bag [bags]
  (->> (str/split bags #",")
       (map str/trim)
       (map #(str/split % #" "))
       (map (fn [[n c]] [(keyword c) (parse-long n)]))
       (into {})))

(defn parse-line [line]
  (let [parts (str/split line #":|;")
        bags (map parse-bag (drop 1 parts))]
    bags))

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(def bag {:red 12 :green 13 :blue 14})

(defn possible? [game]
  (every?
    (fn [{:keys [red green blue] :or {red 0 green 0 blue 0}}]
      (and (<= red (bag :red))
           (<= green (bag :green))
           (<= blue (bag :blue))))
    game))

(defn required-cubes [game]
  (reduce
    (fn [res {:keys [red green blue] :or {red 0 green 0 blue 0}}]
      (-> res
        (update :red max red)
        (update :green max green)
        (update :blue max blue)))
    {:red 0 :green 0 :blue 0}
    game))

(defn power [game]
  (apply * (vals (required-cubes game))))

(defn solve-part1 [input]
  (->> input
       (map-indexed vector)
       (filter (fn [[_i game]] (possible? game)))
       (map (fn [[i _game]] (inc i)))
       (apply +)))

(defn solve-part2 [input]
  (->> input
       (map power)
       (apply +)))
