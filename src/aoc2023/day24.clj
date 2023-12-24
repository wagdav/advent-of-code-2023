(ns aoc2023.day24
  (:require [clojure.string :as str]))

(def example-input "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

(defn parse-input [input]
  (->> (re-seq #"-?\d+" input)
       (mapv parse-long)
       (partition 6)))

; y = m*x - m*x0 + y0
(defn ->slope-intercept [[px py _ vx vy _]]
  (let [slope (/ vy vx)
        intercept (- py (* slope px))]
    [slope intercept]))

(defn intersection [l1 l2]
  (let [[a c] (->slope-intercept l1)
        [b d] (->slope-intercept l2)]
    (when (not= a b)
      (let [x (/ (- d c) (- a b))
            y (+ (* a x) c)]
        [x y]))))

(defn on-future-path? [[px py _ vx vy _] [x y]]
  (cond
    (and (pos? vx) (pos? vy)) (and (< px x) (< py y))
    (and (neg? vx) (neg? vy)) (and (< x px) (< y py))
    (and (pos? vx) (neg? vy)) (and (< px x) (< y py))
    (and (neg? vx) (pos? vy)) (and (< x px) (< py y))))

(defn solve-part1 [input]
  (let [lo 200000000000000
        hi 400000000000000]
    (/
      (count
        (for [l1 input
              l2 input
              :let [pi (intersection l1 l2)
                    [xi yi] pi]
              :when (and pi
                         (on-future-path? l1 pi)
                         (on-future-path? l2 pi)
                         (<= lo xi hi)
                         (<= lo yi hi))]
          (mapv float pi)))
      2)))

; find: xr yr zr @ vrx vry vrz
;   and t1, t2, tN collision times
;
; for x
; rock @t             xr + t * vrx
;
; rock @t1 hits s1    xr + t1 * vrx = x1 + v1x * t1
;    =>  t1 * (vrx - v1x) = - (xr - x1)
;        t1 * (vry - v1y) = - (yr - y1)
;        t1 * (vrz - v1z) = - (xz - z1)
;
; Following the derviation https://github.com/dirk527/aoc2021/blob/main/src/aoc2023/Day24.jpg
; Eliminate t1 and the terms that are common for all hailstones.
;
; Construct four equations in the form of:
;
; (y1 - y2) * vrx  + (x2 - x1) * vry + (v2y - v1y)  * rx  + (v1x - v2x) * ry = y1 * v1x - x1 * v1y - y2 * v2x + x2 * v2y
;
; for [vrx, vry, rx, ry] and a second system for [vrz, vry, rz, ry]
;
; The function below constructs the augmented matrix for these two sets of equation.  See the solution with numpy in day24.py.
(defn solve-part2 [input]
  (println
    (for [[[x1 y1 z1 v1x v1y v1z] [x2 y2 z2 v2x v2y v2z]] (take 4 (partition 2 1 input))]
      [(- y1 y2)
       (- x2 x1)
       (- v2y v1y)
       (- v1x v2x)
       (+ (* y1 v1x) (- (* x1 v1y)) (- (* y2 v2x)) (* x2 v2y))]))
  (println
    (for [[[x1 y1 z1 v1x v1y v1z] [x2 y2 z2 v2x v2y v2z]] (take 4 (partition 2 1 input))]
      [(- y1 y2)
       (- z2 z1)
       (- v2y v1y)
       (- v1z v2z)
       (+ (* y1 v1z) (- (* z1 v1y)) (- (* y2 v2z)) (* z2 v2y))])))
