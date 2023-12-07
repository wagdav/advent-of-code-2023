(ns aoc2023.day07-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day07 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
")

(deftest works
  (testing "with example input"
    (is (= 6440 (solve-part1 (parse-input example-input))))
    (is (= 5905 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day07.txt")))]
      (is (= 249204891 (solve-part1 input)))
      (is (= 249666369 (solve-part2 input))))))
