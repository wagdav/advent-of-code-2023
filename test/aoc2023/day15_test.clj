(ns aoc2023.day15-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day15 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(deftest works
  (testing "with example input"
    (is (= 1320 (solve-part1 (parse-input example-input))))
    (is (= 145 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day15.txt")))]
      (is (= 521341 (solve-part1 input)))
      (is (= 252782 (solve-part2 input))))))
