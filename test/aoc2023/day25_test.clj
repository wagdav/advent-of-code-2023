(ns aoc2023.day25-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day25 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")

(deftest works
  (testing "with example input"
    #_(is (= 54 (solve-part1 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day25.txt")))]
      (is (= 538368 (solve-part1 input))))))
