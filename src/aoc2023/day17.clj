(ns aoc2023.day17
  (:require [clojure.string :as str]
            [aoc2023.search :as search]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        grid (into {} (for [[i row] (map-indexed vector (str/split-lines input))
                            [j n] (map-indexed vector row)]
                        [[i j] (parse-long (str n))]))]
    {:grid grid
     :rows (count lines)
     :cols (count (first lines))}))

(def directions [[0 1] [1 0] [0 -1] [-1 0]]) ; right, down, left, up

(defn move [pos dir]
  (mapv + pos (get directions dir)))

(defn turns [d]
  [(-> d inc (mod 4)) (-> d dec (mod 4))])

(defn manhattan [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (apply +)))

(defn crucible-actions [grid {:keys [pos directions-taken]}]
  (let [d (first directions-taken)
        allowed-dirs (if (and (= 3 (count directions-taken))
                              (= 1 (count (distinct directions-taken))))
                       (turns d)
                       (if (empty? directions-taken)
                         (range 4)
                         (conj (turns d) d)))]
    (for [d allowed-dirs
          :when (grid (move pos d))]
      d)))

(defn solve-part1 [{:keys [grid rows cols]}]
  (:path-cost
    (search/A*
      (reify search/Problem
        (actions [_ state]
          (crucible-actions grid state))
        (goal? [_ state]
          (= [(dec rows) (dec cols)] (:pos state)))
        (initial-state [_]
          {:pos [0 0]
           :directions-taken '()})
        (result [_ {:keys [pos directions-taken]} action]
          {:pos (move pos action)
           :directions-taken (take 3 (conj directions-taken action))})
        (step-cost [_ {:keys [pos]} action]
          (grid (move pos action))))
      (fn [node] (manhattan [(dec rows) (dec cols)] (get-in node [:state :pos]))))))

(defn ultra-crucible-actions [grid {:keys [pos dir straight]}]
  (let [min-straight 4
        max-straight 10
        dirs (cond
               (nil? dir)
               (range 4)

               (< straight min-straight)
               [dir]

               (and (<= min-straight straight) (< straight max-straight))
               (conj (turns dir) dir)

               :else
               (turns dir))]
    (for [d dirs
          :when (grid (move pos d))]
      d)))

(defn heat-loss2 [{:keys [grid rows cols]}]
  (search/A*
    (reify search/Problem
      (actions [_ state]
        (ultra-crucible-actions grid state))
      (goal? [_ state]
        (and (= [(dec rows) (dec cols)] (:pos state))
             (<= 4 (:straight state))))
      (initial-state [_]
        {:pos [0 0]
         :straight 1
         :dir nil})
      (result [_ {:keys [pos dir straight]} action]
        {:pos (move pos action)
         :straight (if (= action dir) (inc straight) 1)
         :dir action})
      (step-cost [_ {:keys [pos]} action]
        (grid (move pos action))))
    (fn [node] (manhattan [(dec rows) (dec cols)] (get-in node [:state :pos])))))

(defn solve-part2 [input]
  (:path-cost (heat-loss2 input)))

(comment
  (defn viz [{:keys [grid rows cols]} res]
    (let [p (set (map :pos (:path res)))]
      (doseq [i (range rows)]
        (doseq [j (range cols)]
           (if (p [i j])
             (print "#")
             (print (grid [i j]))))
        (println))
      (println "cost=" (:path-cost res))))

  (require '[aoc2023.day17-test :as t])

  (let [input (parse-input t/example-input)
        res (heat-loss2 input)]
    (viz input res) ; 94
    res)

  (let [input (parse-input t/example-input2)
        res (heat-loss2 input)]
    (viz input res) ; 71
    res))
