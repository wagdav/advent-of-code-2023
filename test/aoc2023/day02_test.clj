(ns aoc2023.day02-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day02 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(deftest works
  (testing "with example input"
    (is (= 8 (solve-part1 (parse-input example-input))))
    (is (= 2286 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day02.txt")))]
      (is (= 2101 (solve-part1 input)))
      (is (= 58269 (solve-part2 input))))))
