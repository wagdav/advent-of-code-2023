(ns aoc2023.day06-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day06 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "Time:      7  15   30
Distance:  9  40  200
")

(deftest works
  (testing "with example input"
    (is (= 288 (solve-part1 (parse-input example-input))))
    (is (= 71503 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day06.txt")))]
      (is (= 625968 (solve-part1 input)))
      (is (= 43663323 (solve-part2 input))))))
