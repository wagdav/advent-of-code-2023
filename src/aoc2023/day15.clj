(ns aoc2023.day15
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (str/split (str/trim input) #","))

(defn elf-hash [s]
  (reduce
    (fn [res c]
      (-> (int c)
          (+ res)
          (* 17)
          (rem 256)))
    0
    s))

(defn parse-instruction [s]
  (let [[label op f] (re-seq #"\w+|[=-]|\d+" s)]
    (cond-> [op label]
            f (conj (parse-long f)))))

(defn elf-hash-map [boxes ins]
  (let [[op label f] (parse-instruction ins)]
    (update boxes (elf-hash label)
      (fn [box]
        (case op
          "=" (assoc box label f)
          "-" (dissoc box label))))))

(defn focusing-power [boxes]
  (apply +
    (for [[box-id lenses] boxes
          [i [_ f]] (map-indexed vector lenses)]
      (* (inc box-id) (inc i) f))))

(defn solve-part1 [input]
  (apply + (map elf-hash input)))

(defn solve-part2 [input]
  (focusing-power (reduce elf-hash-map {} input)))
