(ns aoc2023.day18-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day18 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
")

(deftest works
  (testing "with example input"
    (is (= 62 (solve-part1 (parse-input example-input))))
    (is (= 952408144115 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day18.txt")))]
      (is (= 40761 (solve-part1 input)))
      (is (= 106920098354636 (solve-part2 input))))))
