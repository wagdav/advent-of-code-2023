(ns aoc2023.day03
  (:require [clojure.string :as str]))

(defn parse-symbols [lines]
  (into {}
    (for [[row line] (map-indexed vector lines)
          [col c] (map-indexed vector line)
          :when (and (not (Character/isDigit c))
                     (not= c \.))]
      [[row col] c])))

(defn parse-number-row [row line]
  (second
    (reduce
      (fn [[col numbers] s]
        [(+ col (count s))
         (if (every? #(Character/isDigit %) s)
           (conj numbers [[row col] (str/join s)])
           numbers)])
      [0 []]
      (partition-by #(Character/isDigit %) line))))

(defn parse-numbers [lines]
  (into {}
    (for [[row line] (map-indexed vector lines)
          n (parse-number-row row line)]
      n)))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:symbols (parse-symbols lines)
     :numbers (parse-numbers lines)}))

(defn around [[[row col] digits]]
  (set
    (concat
      ; line above and below
      (for [c (range (dec col) (inc (+ col (count digits))))
            r [(dec row) (inc row)]]
        [r c])
      ; same line
      [[row (dec col)] [row (+ col (count digits))]])))

(defn solve-part1 [{:keys [symbols numbers]}]
  (let [symbol-coords (set (keys symbols))]
    (->> numbers
         (keep #(when (some symbol-coords (around %))
                  (parse-long (second %))))
         (apply +))))

(defn solve-part2 [{:keys [symbols numbers]}]
  (let [gear-coords (keep (fn [[c s]] (when (= s \*) c)) symbols)
        neigbours-map (into {} (for [n numbers] [n (around n)]))]
    (apply +
      (for [g gear-coords
            :let [adjacent (for [n numbers
                                 :when (contains? (neigbours-map n) g)]
                             (parse-long (second n)))]
            :when (= (count adjacent) 2)]
        (apply * adjacent)))))
