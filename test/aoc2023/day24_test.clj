(ns aoc2023.day24-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day24 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

(deftest works
  (testing "with example input"
    (is (= 13910 (solve-part1 (parse-input example-input))))
    (is (= 47 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day24.txt")))]
      (is (= 13910 (solve-part1 input)))
      (is (= 618534564836937 (solve-part2 input))))))
