(ns aoc2023.day09-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day09 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(deftest works
  (testing "with example input"
    (is (= 114 (solve-part1 (parse-input example-input))))
    (is (= 2 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day09.txt")))]
      (is (= 1684566095 (solve-part1 input)))
      (is (= 1136 (solve-part2 input))))))
