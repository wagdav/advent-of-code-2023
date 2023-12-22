(ns aoc2023.day22-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day22 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(deftest works
  (testing "with example input"
    (is (= 5 (solve-part1 (parse-input example-input))))
    (is (= 7 (solve-part2 (parse-input example-input)))))

  ; works but super slow
  #_(testing "with real input"
      (let [input (parse-input (slurp (io/resource "day22.txt")))]
        (is (= 428 (solve-part1 input)))
        (is (= 35654 (solve-part2 input))))))
