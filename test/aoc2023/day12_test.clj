(ns aoc2023.day12-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day12 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
")

(deftest works
  (testing "with example input"
    (is (= 21 (solve-part1 (parse-input example-input))))
    (is (= 525152 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day12.txt")))]
      (is (= 6949 (solve-part1 input)))
      (is (= 51456609952403 (solve-part2 input))))))
