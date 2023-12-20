(ns aoc2023.day20
  (:require [clojure.string :as str]))

(defn parse-module [line]
  (let [parts (re-seq #"[&%]|\w+" line)
        t (when-let [n (#{"%" "&"} (first parts))] (keyword n))
        n (if t (second parts) (first parts))]
    [n (cond-> {:name n :outputs (drop (if t 2 1) parts)}
               t (assoc :type t))]))

(defn init [modules]
  (let [inputs (apply merge-with into (for [[n m] modules
                                            o (:outputs m)]
                                       {o [n]}))]
    (into {}
      (map
        (fn [[n m]]
          (case (:type m)
            :% [n (assoc m :state :off)]
            :& [n (assoc m :memory (into {} (for [i (inputs n)] [i :low])))]
            [n m]))
        modules))))

(defn emit [module signal]
  (for [o (:outputs module)] [(:name module) signal o]))

(defn output [modules input port signal]
  (let [{:keys [type state memory] :as module} (modules input)]
    (case type
      :% (case signal
           :high [module '()]
           :low  (case state
                   :off  [(assoc module :state :on)  (emit module :high)]
                   :on   [(assoc module :state :off) (emit module :low)]))
      :& (let [mem (assoc memory port signal)
               m (assoc module :memory mem)]
           (if (= #{:high} (set (vals mem)))
             [m (emit m :low)]
             [m (emit m :high)]))
      [module (emit module signal)])))

(defn parse-input [input]
  (let [modules (->> input
                     str/split-lines
                     (map parse-module))]
    (init (into {} modules))))

(defn button [modules]
  (let [[_ start] (output modules "broadcaster" "button" :low)]
    (loop [lows 1, highs 0, ms modules, signals (vec start)]
      (if (seq signals)
        (let [[src sig tgt] (first signals)
              [new-m new-sigs] (output ms tgt src sig)]
          (recur (if (= sig :low) (inc lows) lows)
                 (if (= sig :high) (inc highs) highs)
                 (assoc ms tgt new-m)
                 (into (vec (rest signals)) new-sigs)))
        [[lows highs] ms]))))

(defn solve-part1 [input]
  (loop [i 0, lows 0, highs 0, s input]
    (if (= i 1000)
      (* lows highs)
      (let [[[l h] ss] (button s)]
        (recur (inc i)
               (+ lows l)
               (+ highs h)
               ss)))))

(defn button2 [modules to-rx i cycles]
  (let [[_ start] (output modules "broadcaster" "button" :low)]
    (loop [cs cycles, ms modules, signals (vec start)]
      (let [rx-memory (get-in ms [to-rx :memory])
            next-cs (into cs (for [[k v] rx-memory
                                   :when (= v :high)]
                               [k i]))]
        (if (seq signals)
          (let [[src sig tgt] (first signals)
                [new-m new-sigs] (output ms tgt src sig)]
            (recur next-cs
                   (assoc ms tgt new-m)
                   (into (vec (rest signals)) new-sigs)))
          [cs ms])))))

(defn solve-part2 [input]
  (let [to-rx (some (fn [[n m]]
                      (when (= (:outputs m) ["rx"]) n)) input)
        slots (count (get-in input [to-rx :memory]))]
    (loop [i 1, state input, cycles {}]
      (let [[next-cycles next-state] (button2 state to-rx i cycles)]
        (if (= (count next-cycles) slots)
          (apply * (vals next-cycles))
          (recur (inc i) next-state next-cycles))))))
