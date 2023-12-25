(ns aoc2023.day25
  (:require [clojure.string :as str]
            [aoc2023.search :as search]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-seq #"\w+" %))
       (map (fn [line] [(first line) (next line)]))
       (into {})))

(defn reverse-connections [components]
  (update-vals
    (group-by first
      (for [c (keys components)
            n (components c)]
        [n c]))
    #(map second %)))

(defn with-reverse [components]
  (merge-with concat components (reverse-connections components)))

(defn flood [components a]
  (loop [explored #{} frontier #{a}]
    (let [new-frontier (set (for [p frontier
                                  c (components p)
                                  :when (not (explored c))]
                              c))]
      (if (empty? new-frontier)
        (count explored)
        (recur (into explored new-frontier) new-frontier)))))

(defn disconnect [components [a b]]
  (-> components
      (update a #(remove #{b} %))
      (update b #(remove #{a} %))))

; Solved manually: convert the input into a .dot file and run "neato -T svg day25.dot > day25.svg"))
(defn solve-part1 [input]
  (let [disconnected (-> (with-reverse input)
                         (disconnect ["mmr" "znk"])
                         (disconnect ["rnx" "ddj"])
                         (disconnect ["vcq" "lxb"]))]
    (* (flood disconnected "lxb")
       (flood disconnected "vcq"))))

(comment
  (require '[aoc2023.day25-test :as t])
  (let [disconnected (-> (parse-input t/example-input)
                         (with-reverse)
                         (disconnect ["hfx" "pzl"])
                         (disconnect ["bvb" "cmg"])
                         (disconnect ["nvd" "jqt"]))]
     (* (flood disconnected "hfx") (flood disconnected "pzl"))))
