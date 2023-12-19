(ns aoc2023.day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-rule [line]
  (let [[r target] (str/split line #":")
        [p c limit] (re-seq #"[asxm]|\d+|[<>]" line)]
    {:category p
     :condition c
     :limit (parse-long limit)
     :target target}))

(defn parse-workflow [line]
  (let [[n ws] (str/split line #"[{}]")
        rules (str/split ws #",")]
    [n {:rules (map parse-rule (butlast rules))
        :default (last rules)}]))

(defn parse-part [line]
  (let [ps (re-seq #"[asxm]|\d+" line)]
    (into {}
      (for [[n v] (partition 2 ps)]
        [n (parse-long v)]))))

(defn parse-input [input]
  (let [[b1 b2] (str/split input #"\n\n")
        ws (map parse-workflow (str/split-lines b1))
        ps (map parse-part (str/split-lines b2))]
    {:workflows (into {} ws)
     :parts ps}))

(defn run-workflow [{:keys [rules default]} part]
  (loop [rs rules]
    (if (seq rs)
      (let [{:keys [condition category limit target]} (first rs)
            cond-fn ({">" > "<" <} condition)]
        (if (cond-fn (get part category) limit)
          target
          (recur (rest rs))))
      default)))

(defn run [workflows part]
  (loop [n "in", w (workflows "in")]
    (let [result (run-workflow w part)]
      (if (#{"R" "A"} result)
        result
        (recur result (workflows result))))))

(defn solve-part1 [{:keys [parts workflows]}]
  (->> parts
       (filter #(= "A" (run workflows %)))
       (map vals)
       (map #(apply + %))
       (apply +)))

(defn run2 [workflows ranges rules]
  (loop [accepted [], workqueue [[ranges rules]]]
    (if (empty? workqueue)
      accepted
      (let [[part {:keys [rules default] :as workflow}] (first workqueue)
            [dst to-target to-next]
            (if (empty? rules)
              [default part nil] ; no more rules, send part to default
              (let [{:keys [condition category limit target]} (first rules)
                    [lo hi] (part category)]
                (case condition
                  "<"  (cond (< lo hi limit)
                             ; send whole to target, nothing to rest of the workflow
                             [target part nil]
                             (< lo limit hi)
                             ; send matching to target, complement to rest of the workflow
                             [target (assoc part category [lo (dec limit)])
                                     (assoc part category [limit hi])]
                             (< limit lo hi)
                             ; send whole range to rest of the pipeline
                             [target nil part])
                  ">"  (cond (> hi lo limit)
                             ; send whole to target, nothing to rest of the workflow
                             [target part nil]
                             (> hi limit lo)
                             ; send matching to target, complement to rest of the workflow
                             [target (assoc part category [(inc limit) hi]),
                                     (assoc part category [lo limit])]
                             (> limit hi lo)
                             ; send whole range to rest of the pipeline
                             [target nil part]))))]
        (recur (if (= dst "A")
                 (conj accepted to-target)
                 accepted)
               (cond-> (rest workqueue)
                 to-next
                 (conj [to-next (assoc workflow :rules (rest rules))])

                 (and to-target (not (#{"A" "R"} dst)))
                 (conj [to-target (workflows dst)])))))))

(defn solve-part2 [{:keys [workflows]}]
  (let [p {"x" [1 4000], "m" [1 4000], "a" [1 4000], "s" [1 4000]}
        w (workflows "in")]
    (apply +
      (for [p (run2 workflows p w)]
        (apply *
          (for [[lo hi] (vals p)]
            (- (inc hi) lo)))))))
