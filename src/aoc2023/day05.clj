(ns aoc2023.day05
  (:require [clojure.string :as str]))

(def example-input "seeds: 55 13 79 14

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
0 11 42
42 0 7
49 53 8
57 7 4

water-to-light map:
18 25 70
88 18 7

light-to-temperature map:
45 77 23
68 64 13
81 45 19

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
")

(defn parse-map [m]
   (->> (re-seq #"\d+" m) (map parse-long) (partition 3)))

(defn parse-input [input]
  (let [[s & ms] (str/split input #"\n\n")]
    {:seeds (map parse-long (re-seq #"\d+" s))
     :maps (map parse-map ms)}))

(defn map-value [x rs]
  (or
    (some (fn [[dst src length]]
            (when (<= src x (+ src length -1))
              (+ dst (- x src))))
          rs)
    x))

(defn location [ms seed]
  (reduce map-value seed ms))

(defn solve-part1 [{:keys [seeds maps]}]
  (apply min (map (partial location maps) seeds)))

(solve-part1 (parse-input example-input))

(defn map-range* [[s l] [dst src len]]
  (cond
    (= [s l] [src len])
    [dst len]

    ; 1. [input] [src] -> [input]
    (and (< s src) (< (+ s l -1) src))
    [:1 s l]

    ; 2. [src] [input] -> [input]
    (and (< src s) (< (+ src len -1) s))
    [:2 s l]

    ; 3. [input]
    ;        [src]
    (and (< s src) (<= src (+ s l -1)))
    [:3 dst (- (+ s l) src)]

    ; 4.   [input]
    ;    [src]
    (and (<= src s) (<= s (+ src len -1)))
    [:4 (+ dst (- s src)) (- (+ src len) s)]

    ; 5. [input]
    ;     [src]
    (and (< s src) (< (+ src len -1) s))
    [:5 dst len]

    ; 6.   [input]
    ;    [   src   ]
    (and (< src s) (< (+ s l -1) (+ src len -1)))
    [:6 (+ dst (- s src)) l]

    :else
    (assert false)))

(defn map-range [a b]
  (drop 1 (map-range* a b)))

(comment
  (map-range [0 2] [10 2 2])
  (map-range [2 2] [10 0 2])
  (map-range [0 2] [10 1 2])
  (map-range [2 2] [10 1 2])
  (map-range [1 4] [10 2 2]))

(defn solve-part2-naive [{:keys [seeds maps]}]
  (let [seed-ranges (partition 2 seeds)]
    (apply min
      (for [[lo length] seed-ranges
            s (range lo (+ lo length))]
        (location maps s)))))

(defn solve-part2 [{:keys [seeds maps]}])
