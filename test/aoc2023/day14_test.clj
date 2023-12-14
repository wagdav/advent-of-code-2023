(ns aoc2023.day14-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day14 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
")

(deftest works
  (testing "with example input"
    (is (= 136 (solve-part1 (parse-input example-input))))
    (is (= 64 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day14.txt")))]
      (is (time (= 109638 (solve-part1 input))))
      #_(is (time (= 102657 (solve-part2 input))))))) ; slow, runs in ~36s
