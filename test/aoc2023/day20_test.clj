(ns aoc2023.day20-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day20 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a ")

(deftest works
  (testing "with example input"
    (is (= 32000000 (solve-part1 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day20.txt")))]
      (is (= 818723272 (solve-part1 input)))
      (is (= 243902373381257 (solve-part2 input))))))
