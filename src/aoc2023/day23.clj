(ns aoc2023.day23
  (:require [clojure.string :as str]
            [aoc2023.search :as search]))

(def example-input "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:grid lines
     :rows (count lines)
     :cols (count (first lines))}))

(parse-input example-input)

(def directions {\> [0 1], \v [1 0], \< [0 -1], \^ [-1 0]})

(def allowed-tile (conj (set (keys directions)) \.))

(defn move [pos dir]
  (mapv + pos (get directions dir)))

(defn actions [grid pos]
  (for [d (keys directions)
        :let [p (move pos d)]
        :when (and (allowed-tile (get-in grid pos))
                   (allowed-tile (get-in grid p)))]
    d))

(defn crossings [grid]
  (for [[i row] (map-indexed vector grid)
        [j c] (map-indexed vector row)
        :when (< 2 (count (actions grid [i j])))]
    [i j]))

(defn distance [grid crossings start end]
  (:path-cost
    (let [disallowed (set (remove #{end} crossings))]
      (search/breadth-first
        (reify search/Problem
          (actions [_ {:keys [pos explored]}]
            (for [d (keys directions)
                  :let [p (move pos d)]
                  :when (and (allowed-tile (get-in grid p))
                             (not (disallowed p))
                             (not (explored p)))]
              d))
          (goal? [_ {:keys [pos]}] (= pos end))
          (initial-state [_]
            {:pos start
             :explored #{}})
          (result [_ {:keys [pos explored]} action]
            {:pos (move pos action)
             :explored (conj explored pos)})
          (step-cost [_ _ _]
            -1))))))

(defn solve-part1 [input])

(defn walk [{:keys [grid rows cols]}]
  (let [start [0 1]
        end [(dec rows) (dec (dec cols))]
        cs (into (crossings grid) [start end])
        g (into {}
            (for [a cs]
              [a (into {} (for [b cs
                                :let [d (distance grid cs a b)]
                                :when (and d (not= a b))]
                            [b d]))]))]
    (println g)
    (search/depth-first
      (reify search/Problem
        (actions [_ {:keys [pos explored]}]
          (for [d (keys (g pos))
                :when (not (explored d))]
            d))
        (goal? [_ {:keys [pos]}] (= pos end))
        (initial-state [_] {:pos start, :explored #{}})
        (result [_ {:keys [pos explored]} action]
          {:explored (conj explored pos)
           :pos action})
        (step-cost [_ {:keys [pos]} action]
          (get-in g [pos action]))))))

; 6538?
(defn solve-part2 [input]
  (:path-cost (walk input)))

(-> (parse-input example-input)
    :grid
    (actions [5 3]))

(println (solve-part2 (parse-input example-input)))

(comment)
(require '[clojure.java.io :as io])
(let [input (parse-input (slurp (io/resource "day23.txt")))]
  (println (solve-part2 input)))

(comment
  (println (solve-part1 (parse-input example-input)))

  (defn viz [{:keys [grid rows cols]} res]
    (let [p (set (map :pos (:path res)))]
      (doseq [i (range rows)]
        (doseq [j (range cols)]
           (if (p [i j])
             (print "O")
             (print (get-in grid [i j]))))
        (println))
      (println "cost=" (:path-cost res))))

  (let [input (parse-input example-input)
        res (walk input)]
    (viz input res)
    (map :path-cost res))

  (let [input (parse-input (slurp (io/resource "day23.txt")))
        res (walk input)]
    (viz input res)
    (map :path-cost res)))
