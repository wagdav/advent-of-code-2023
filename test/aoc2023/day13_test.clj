(ns aoc2023.day13-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day13 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(deftest works
  (testing "with example input"
    (is (= 405 (solve-part1 (parse-input example-input))))
    (is (= 400 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day13.txt")))]
      (is (= 29846 (solve-part1 input)))
      (is (= 25401 (solve-part2 input))))))
