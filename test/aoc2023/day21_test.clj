(ns aoc2023.day21-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day21 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(deftest works
  (testing "with example input"
    (is (= 42 (solve-part1 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day21.txt")))]
      (is (= 3716 (solve-part1 input)))
      #_(is (= 616583483179597 (solve-part2 input)))))) ; slow
