(ns aoc2023.day08-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day08 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input1 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(def example-input2 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(deftest works
  (testing "with example input"
    (is (= 6 (solve-part1 (parse-input example-input1))))
    (is (= 6 (solve-part2 (parse-input example-input2)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day08.txt")))]
      (is (= 20777 (solve-part1 input)))
      (is (= 13289612809129 (solve-part2 input))))))
