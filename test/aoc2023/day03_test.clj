(ns aoc2023.day03-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day03 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
")

(deftest works
  (testing "with example input"
    (is (= 4361 (solve-part1 (parse-input example-input))))
    (is (= 467835 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day03.txt")))]
      (is (= 525181 (solve-part1 input)))
      (is (= 84289137 (solve-part2 input))))))
