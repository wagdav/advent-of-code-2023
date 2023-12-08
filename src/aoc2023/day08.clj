(ns aoc2023.day08
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[turns & nodes] (re-seq #"\w+" input)]
   {:turns turns
    :nodes (->> nodes
                (partition 3)
                (map (fn [[f l r]] [f [l r]]))
                (into {}))
    :start (filter #(str/ends-with? % "A") nodes)}))

(defn solve-part1 [{:keys [turns nodes]}]
  (reduce
    (fn [[steps pos] turn]
      (if (= pos "ZZZ")
        (reduced steps)
        (let [[l r] (nodes pos)]
          [(inc steps) ({\L l \R r} turn)])))
    [0 "AAA"]
    (cycle turns)))

(defn lcm
  "Least common multiple of a and b"
  [a b]
  (.divide
    (.multiply (biginteger a) (biginteger b))
    (.gcd (biginteger a) (biginteger b))))

(defn solve-part2 [{:keys [turns nodes start]}]
  (loop [steps 0, ps start, turns (cycle turns), periods {}]
   (if (= (count periods) (count start))
     (reduce lcm (vals periods))
     (let [to (for [[l r] (map nodes ps)] ({\L l \R r} (first turns)))
           return (into {}
                        (for [[i p] (map-indexed vector ps)
                              :when (str/ends-with? p "Z")]
                          [i steps]))]
       (recur (inc steps) to (rest turns) (merge periods return))))))
