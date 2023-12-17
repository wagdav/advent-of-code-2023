(ns aoc2023.day16-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day16 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(deftest works
  (testing "with example input"
    (let [example-input (parse-input (slurp (io/resource "day16-example.txt")))]
      (is (= 46 (solve-part1 example-input)))
      (is (= 51 (solve-part2 example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day16.txt")))]
      (is (= 6994 (solve-part1 input)))
      (is (= 7488 (solve-part2 input))))))
