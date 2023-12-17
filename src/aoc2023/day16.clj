(ns aoc2023.day16
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(def directions {:right [0 1], :down [1 0], :left [0 -1], :up [-1 0]})

(defn move [p d]
  (mapv + p (directions d)))

(defn reflect [layout [pos dir]]
  (let [new-dirs (case (get-in layout pos)
                    \. [dir]
                    \- (if (#{:left :right} dir)
                         [dir]
                         [:left :right])
                    \| (if (#{:up :down} dir)
                         [dir]
                         [:up :down])
                    \\ [({:right :down, :down :right, :left :up, :up :left} dir)]
                    \/ [({:right :up, :down :left, :left :down, :up :right} dir)])]
    (for [d new-dirs
          :let [new-pos (move pos d)]
          :when (get-in layout new-pos)]
      [new-pos d])))

(defn energize [layout beam]
  (loop [beams [beam], seen #{}, splits #{}]
    (if (seq beams)
      (let [beam (first beams)
            cur (first beam)
            new-beam (reflect layout beam)
            new-seen (conj seen cur)]
       (case (count new-beam)
         ; beam terminated
         0 (recur (rest beams) new-seen splits)
         ; beam keeps going
         1 (recur (cons (first new-beam) (rest beams)) new-seen splits)
         ; beam splits
         2 (if (splits cur)
             (recur (rest beams) new-seen splits)
             (recur (into new-beam (rest beams))
                    new-seen
                    (conj splits cur)))))
      (count seen))))

(defn solve-part1 [input]
  (energize input [[0 0] :right]))

(defn solve-part2 [input]
  (let [rows (count input)
        cols (count (first input))]
    (apply max
      (concat
        (for [r (range rows)
              beam (vector [[r          0] :right]
                           [[r (dec cols)] :left])]
          (energize input beam))
        (for [c (range cols)
              beam (vector [[         0 c] :down]
                           [[(dec rows) c] :up])]
          (energize input beam))))))
