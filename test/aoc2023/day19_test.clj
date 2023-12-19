(ns aoc2023.day19-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2023.day19 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
")

(deftest works
  (testing "with example input"
    (is (= 19114 (solve-part1 (parse-input example-input))))
    (is (= 167409079868000 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day19.txt")))]
      (is (= 509597 (solve-part1 input)))
      (is (= 143219569011526 (solve-part2 input))))))
