(ns aoc2023.day10
  (:require [clojure.string :as str]))

(def north [-1  0])
(def south [ 1  0])
(def east  [ 0  1])
(def west  [ 0 -1])

(def tiles {\| #{north south}
            \- #{east west}
            \L #{north east}
            \J #{north west}
            \7 #{south west}
            \F #{south east}})

(defn add [p1 p2]
  (mapv + p1 p2))

(defn connected [pipes pos]
  (map #(add pos %) (tiles (get pipes pos))))

(defn fill-start-tile [pipes start]
  (some
    (fn [tile]
      (let [pipes* (assoc pipes start tile)
            [e1 e2] (connected pipes* start)]
        (when (and (contains? (set (connected pipes* e1)) start)
                   (contains? (set (connected pipes* e2)) start))
          pipes*)))
    (keys tiles)))

(defn parse-input [input]
  (let [pipes (into {}
                (for [[r line] (map-indexed vector (str/split-lines input))
                      [c p] (map-indexed vector line)]
                  [[r c] p]))
        start (->> pipes (filter #(= (second %) \S)) ffirst)]
    {:pipes (fill-start-tile pipes start)
     :start start}))

(defn solve-part1 [{:keys [pipes start]}]
  (loop [c 1, frontier (set (connected pipes start)), visited #{start}]
    (let [new-frontier (->> frontier
                            (mapcat #(connected pipes %))
                            (remove visited))]
      (if (empty? new-frontier)
        c
        (recur (inc c) new-frontier (into visited frontier))))))

(def left  {north west, east north, south east, west south})
(def right {north east, east south, south west, west north})

(defn follow
  "From a given position follow the pipes in the direction. Returns the new
  position and new direction."
  [pipes pos dir]
  (let [new-pos (add pos dir)
        [d1 d2] (vec (tiles (pipes new-pos)))]
    [new-pos (if (= (add new-pos d1) pos) d2 d1)]))

(defn trace-sides [pos dir]
  (let [l (add pos (left dir))
        r (add pos (right dir))
        ahead (add pos dir)
        lc (add ahead (left dir))
        rc (add ahead (right dir))]
    [#{l lc} #{r rc}]))

(defn flood [pipes path ps]
  (loop [points (set ps), frontier (set ps)]
    (let [new-frontier (set (for [dir [[0 1] [0 -1] [-1 0] [1 0]]
                                  p frontier
                                  :let [f (add p dir)]
                                  :when (and (pipes f)
                                             (not (path f))
                                             (not (points f)))]
                              f))]
      (if (empty? new-frontier)
        points
        (recur (into points new-frontier) new-frontier)))))

(defn solve-part2 [{:keys [pipes start]}]
  (loop [pos start
         dir (first (tiles (pipes start)))
         path #{start}
         lefts #{}
         rights #{}]
    (let [[next-pos, next-dir] (follow pipes pos dir)]
      (if (= next-pos start)
        (apply min ; Assumption: less tiles inside the loop than outside
          (for [ps [lefts rights]]
            (->> ps
                 (remove path)
                 (flood pipes path)
                 count)))
        (let [[next-lefts next-rights] (trace-sides pos dir)]
          (recur next-pos
                 next-dir
                 (conj path next-pos)
                 (into lefts next-lefts)
                 (into rights next-rights)))))))
