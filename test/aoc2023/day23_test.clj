(ns aoc2023.day23-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day23 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(deftest works
  (testing "with example input"
    (is (nil? (solve-part1 (parse-input example-input))))
    (is (nil? (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day23.txt")))]
      #_(is (= 2362 (solve-part1 input)))
      (is (= 6538 (solve-part2 input))))))
