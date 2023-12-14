(ns aoc2023.day14
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (mapv vec (str/split-lines input)))

(defn rocks [dish]
  (for [i (range (count dish))
        j (range (count (first dish)))
        :when (= \O (get-in dish [i j]))]
    [i j]))

(def directions {:north [-1 0] :south [1 0] :west [0 -1] :east [0 1]})

(defn roll
 ([dish dir]
  (reduce
    (fn [g rock] (roll g rock dir))
    dish
    (rocks dish)))
 ([dish pos dir]
  (let [target (map + pos (directions dir))]
    (assert (= \O (get-in dish pos)))
    (if-let [c (get-in dish target)]
      (if (= \. c)
        (-> dish
            (assoc-in pos \.)
            (assoc-in target \O))
        dish)
      dish))))

(defn tilt [dish direction]
  (loop [g dish]
    (let [new-g (roll g direction)]
      (if (= new-g g)
        g
        (recur new-g)))))

(defn total-load [dish]
  (apply +
    (for [[r _] (rocks dish)]
      (- (count dish) r))))

(defn solve-part1 [input]
  (total-load (tilt input :north)))

(defn spin-cycle [dish cycles]
  (loop [c 1, g dish, sigs {}]
    (if (= c cycles)
      g
      (recur
        (if (sigs g) ; Found a repeated state
          (let [period (- c (sigs g))
                remaining (- cycles c)]
            (+ c (* period (quot remaining period)))) ; Fast-forward
          (inc c))
        (-> g (tilt :north) (tilt :west) (tilt :south) (tilt :east))
        (if (sigs g) {} (assoc sigs g c))))))

(def billion 1000000000)

(defn solve-part2 [input]
  (total-load (spin-cycle input billion)))
