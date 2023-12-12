(ns aoc2023.day12
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map
         (fn [line]
           (let [[springs code] (str/split line #" ")]
             [springs (map parse-long (re-seq #"\d+" code))])))))

(defn match? [pattern v]
  (assert (<= (count v) (count pattern)))
  (loop [vs, v, ps pattern]
   (if (seq vs)
     (if (or (= \? (first ps))
             (= (first ps) (first vs)))
       (recur (rest vs) (rest ps))
       false)
     true)))

(defn arrangements
 ([[pattern groups]]
  (arrangements [pattern groups] (- (count pattern) (apply + groups)) true))
 ([[pattern groups] operational first?]
  (if (seq groups)
    (for [start (->> (range (if first? 0 1) (inc operational))
                     (map #(str/join (repeat % ".")))
                     (map #(apply str %)))
          body (arrangements [(subs pattern (+ (count start) (first groups))) (rest groups)]
                             (- operational (count start))
                             false)
          :let [middle (str/join (repeat (first groups) \#))
                res (str/join [start middle body])]
          :when (match? pattern (str/join [start middle]))]
      res)
    (->> [(str/join (repeat operational \.))]
         (filter #(match? pattern %))))))

(def count-arrangements*
  (memoize
    (fn [[pattern groups] operational first?]
      (if (seq groups)
        (let [starts (->> (range (if first? 0 1) (inc operational))
                          (map #(str/join (repeat % ".")))
                          (map #(str/join [% (str/join (repeat (first groups) \#))]))
                          (filter #(match? pattern %)))]
          (apply +
            (for [s starts]
              (count-arrangements* [(subs pattern (count s)) (rest groups)]
                                   (- operational (- (count s) (first groups)))
                                   false))))
        (->> [(str/join (repeat operational \.))]
             (filter #(match? pattern %))
             count)))))

(defn count-arrangements [[pattern groups]]
  (count-arrangements* [pattern groups] (- (count pattern) (apply + groups)) true))

(defn solve-part1 [input]
  (apply + (map count (map arrangements input))))

(defn unfold [[pattern sig]]
  [(str/join "?" (repeat 5 pattern))
   (apply concat (repeat 5 sig))])

(defn solve-part2 [input]
  (let [unfolded-input (map unfold input)]
    (apply + (map count-arrangements unfolded-input))))
