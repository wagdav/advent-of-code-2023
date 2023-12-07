(ns aoc2023.day07
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map (fn [[cards bid]] [cards (parse-long bid)]))))

(defn five-of-a-kind? [cards]
  (= 1 (count (distinct cards))))

(defn four-of-a-kind? [cards]
  (= (-> cards frequencies vals set) #{4 1}))

(defn full-house? [cards]
  (= (-> cards frequencies vals set) #{3 2}))

(defn three-of-a-kind? [cards]
  (= (-> cards frequencies vals set) #{3 1}))

(defn two-pair? [cards]
  (= (-> cards frequencies vals frequencies) {2 2, 1 1}))

(defn one-pair? [cards]
  (= (-> cards frequencies vals frequencies) {2 1, 1 3}))

(defn high-card? [cards]
  (= (count cards) (count (distinct cards))))

(defn first-different [hand1 hand2]
  (->> (map vector hand1 hand2)
       (drop-while (fn [[c1 c2]] (= c1 c2)))
       first))

(defn hand-type [cards]
  (cond
    (five-of-a-kind? cards)  6
    (four-of-a-kind? cards)  5
    (full-house? cards)      4
    (three-of-a-kind? cards) 3
    (two-pair? cards)        2
    (one-pair? cards)        1
    (high-card? cards)       0))

(def hand-type-with-jokers
  (memoize
    (fn [cards]
      (->> "23456789TQKA"
           (map #(str/replace cards \J %))
           (map hand-type)
           (apply max)))))

(defn sort-hands [cards card-value-fn hand-type-fn]
  (sort-by
   (fn [line] [(hand-type-fn (first line)) (first line)])
   (fn [[t1 c1] [t2 c2]]
     (if (not= t1 t2)
       (compare t1 t2)
       (->> (first-different c1 c2)
            (map card-value-fn)
            (apply compare))))
   cards))

(defn winnings [sorted]
  (apply +
    (for [[r [_ b]] (map-indexed vector sorted)]
      (* (inc r) b))))

(defn solve-part1 [input]
  (let [card-value (zipmap "23456789TJQKA" (range))]
    (winnings (sort-hands input card-value hand-type))))

(defn solve-part2 [input]
  (let [card-value (zipmap "J23456789TQKA" (range))] ; J cards are now the weakest
    (winnings (sort-hands input card-value hand-type-with-jokers))))
