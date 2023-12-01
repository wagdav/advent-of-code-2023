(ns aoc2023.day01-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day01 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input1 "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def example-input2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(deftest works
  (testing "with example input"
    (is (= 142 (solve-part1 (parse-input example-input1))))
    (is (= 281 (solve-part2 (parse-input example-input2)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day01.txt")))]
      (is (= 55477 (solve-part1 input)))
      (is (= 54431 (solve-part2 input))))))
