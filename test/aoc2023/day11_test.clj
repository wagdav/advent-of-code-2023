(ns aoc2023.day11-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day11 :refer [parse-input distances solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
")

(deftest works
  (testing "with example input"
    (is (= 374 (solve-part1 (parse-input example-input))))
    (is (= 1030 (distances (parse-input example-input) 10)))
    (is (= 8410 (distances (parse-input example-input) 100))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day11.txt")))]
      (is (= 9233514 (solve-part1 input)))
      (is (= 363293506944 (solve-part2 input))))))
