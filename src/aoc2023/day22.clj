(ns aoc2023.day22
  (:require [clojure.string :as str]
            [clojure.set]))

(def example-input "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn parse-cube [line]
  (let [[x1 y1 z1 x2 y2 z2] (map parse-long (re-seq #"\d+" line))]
    (assert (and (<= x1 x2) (<= y1 y2) (<= z1 z2)))
    [[x1 y1 z1] [x2 y2 z2]]))

(defn parse-input [input]
  (map parse-cube (str/split-lines input)))

(defn height [[[_ _ z1] [_ _ z2]]]
  z1)

(defn on-ground? [cube]
  (= 1 (height cube)))

(defn fall [[[x1 y1 z1] [x2 y2 z2] :as cube]]
  (assert (not (on-ground? cube)))
  [[x1 y1 (dec z1)] [x2 y2 (dec z2)]])

(defn overlap? [[s1 e1] [s2 e2]]
  (assert (and (<= s1 e1) (<= s2 e2)))
  (not (or (< e1 s2) (< e2 s1))))

(defn overlapping-cubes? [[[xs1 ys1 zs1] [xe1 ye1 ze1]]
                          [[xs2 ys2 zs2] [xe2 ye2 ze2]]]
  (and
    (overlap? [xs1 xe1] [xs2 xe2])
    (overlap? [ys1 ye1] [ys2 ye2])
    (overlap? [zs1 ze1] [zs2 ze2])))

(defn disjoint-cubes? [c1 c2]
  (not (overlapping-cubes? c1 c2)))

(defn drop-one [cube cubes]
  (let [wo (remove #{cube} cubes)]
    (loop [c cube]
      (if (on-ground? c)
        c
        (let [f (fall c)]
          (if (every? #(disjoint-cubes? f %) wo)
            (recur f)
            c))))))

(defn drop-cubes [cubes]
  (reduce
    (fn [res c]
      (conj res (drop-one c res)))
    []
    (sort-by
      (fn [[[x1 y1 z1] [z2 y2 z2]]] z1)
      cubes)))

(defn drop-cubes-counting [cubes]
  (reduce
    (fn [[amt res] c]
      (let [dropped (drop-one c res)]
        [(if (= dropped c) amt (inc amt))
         (conj res dropped)]))
    [0 []]
    (sort-by
      (fn [[[x1 y1 z1] [z2 y2 z2]]] z1)
      cubes)))

(defn can-fall? [cubes cube]
  (if (on-ground? cube)
    false
    (if (every? #(disjoint-cubes? (fall cube) %)
                (remove #{cube} cubes))
      true
      false)))

(defn stable? [cubes cube-to-remove]
  (let [wo (->> cubes
                (remove #{cube-to-remove})
                (sort-by (fn [[[x1 y1 z1] [z2 y2 z2]]] z1)))]
    (loop [cs wo]
      (if (empty? cs)
        true
        (if (can-fall? wo (first cs))
          false
          (recur (rest cs)))))))

(defn disintegrate [cubes]
  (loop [d 0, cs cubes]
    (if (seq cs)
      (recur
        (if (stable? cubes (first cs))
          (inc d) d)
        (rest cs))
      d)))

(defn solve-part1 [input]
  (disintegrate (drop-cubes input)))

(defn solve-part2 [input]
  (let [dropped (drop-cubes input)]
    (apply +
      (for [c dropped]
        (first (drop-cubes-counting (remove #{c} dropped)))))))
