(ns aoc2023.day13
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [grids (str/split input #"\n\n")]
    (map str/split-lines grids)))

(defn transpose [v]
  (apply mapv vector v))

(defn repeated [rows]
  (filter (fn [i] (= (get rows i) (get rows (inc i))))
          (range (dec (count rows)))))

(defn mirror? [rows c]
  (assert (= (get rows c) (get rows (inc c))))
  (loop [delta 0]
    (if (or (neg? (- c delta))
            (<= (count rows) (+ c (inc delta))))
      true
      (let [r1 (get rows (- c delta))
            r2 (get rows (+ c (inc delta)))]
        (if (= r1 r2)
          (recur (inc delta))
          false)))))

(defn row-mirrors [rows]
  (for [cc (repeated rows) :when (mirror? rows cc)] cc))

(defn mirrors [grid]
  (set
    (concat
      (for [m (row-mirrors grid)] [:horizontal (inc m)])
      (for [m (row-mirrors (transpose grid))] [:vertical (inc m)]))))

(defn score [[hv pos]]
  (case hv
    :horizontal (* 100 pos)
    :vertical pos))

(defn solve-part1 [input]
  (apply +
    (for [g input]
      (score (first (mirrors g))))))

(defn unsmudge [grid]
  (let [ms (mirrors grid)]
    (first
      (for [i (range (count grid))
            j (range (count (first grid)))
            :let [u (mapv str/join
                      (-> (mapv vec grid)
                          (update-in [i j] {\# \., \. \#})))
                  ums (set (remove ms (mirrors u)))]
            :when (seq ums)]
        (first ums)))))

(defn solve-part2 [input]
  (->> input
       (map unsmudge)
       (map score)
       (apply +)))
