(ns aoc2023.day10-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day10 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "..F7.
.FJ|.
SJ.L7
|F--J
LJ...
")

(def example-four-enclosed "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(def example-four-enclosed-squeeze "..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........")

(def example-large "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(deftest works
  (testing "with example input"
    (is (= 8 (solve-part1 (parse-input example-input))))
    (is (= 4 (solve-part2 (parse-input example-four-enclosed))))
    (is (= 4 (solve-part2 (parse-input example-four-enclosed-squeeze))))
    (is (= 10 (solve-part2 (parse-input example-large)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day10.txt")))]
      (is (= 6979 (solve-part1 input)))
      (is (= 443 (solve-part2 input))))))
