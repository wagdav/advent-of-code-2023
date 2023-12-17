(ns aoc2023.day17-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day17 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
")

(def example-input2 "111111111111
999999999991
999999999991
999999999991
999999999991")

(deftest works
  (testing "with example input"
    (is (= 102 (solve-part1 (parse-input example-input))))
    (is (= 94 (solve-part2 (parse-input example-input))))
    (is (= 71 (solve-part2 (parse-input example-input2)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day17.txt")))]
      (is (= 1128 (solve-part1 input)))
      (is (= 1268 (solve-part2 input))))))
