(ns aoc2023.search
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

(defprotocol Problem
  (actions [this state])
  (goal? [this state])
  (initial-state [this])
  (result [this state action])
  (step-cost [this state action]))

; Russel&Norvig AI (Section 3.1.1 p 78.)
(defrecord Node [state actions path path-cost])

(defn- child-node [problem parent action]
  (let [state (result problem (:state parent) action)]
    (map->Node
      {:state state
       :actions (conj (:actions parent) action)
       :path (conj (:path parent) state)
       :path-cost (+ (:path-cost parent)
                     (step-cost problem (:state parent) action))})))

(defn uniform-cost [problem]
  (let [start (initial-state problem)]
    (loop [explored #{}
           frontier (priority-map-keyfn :path-cost start
                      (map->Node {:state start :actions [] :path [start]
                                  :path-cost 0}))]
      (when-let [[state node] (peek frontier)]
        (if (goal? problem state)
          node
          (recur
            (conj explored state)
            (into (pop frontier) (for [action (actions problem state)
                                       :let [c (child-node problem node action)
                                             s (:state c)]
                                       :when (not (explored s))] [s c]))))))))

(defn best-first [problem f]
  (let [start-state (initial-state problem)
        start-node  (map->Node {:state start-state :actions [] :path [start-state] :path-cost 0})]
    (loop [reached {start-state start-node}
           frontier (priority-map-keyfn f start-state start-node)]
      (when-let [[state node] (peek frontier)]
        (if (goal? problem state)
          node
          (let [children (for [action (actions problem state)
                               :let [c (child-node problem node action)
                                     s (:state c)]
                               :when (or (not (reached s))
                                         (< (:path-cost c) (:path-cost (reached s))))]
                           [s c])]
            (recur
              (into reached children)
              (into (pop frontier) children))))))))

(defn A* [problem h]
  (best-first problem (fn [^Node node] (+ (:path-cost node) (h node)))))

(defn breadth-first [problem]
  (let [start (initial-state problem)]
    (loop [explored #{}
           frontier (conj (clojure.lang.PersistentQueue/EMPTY)
                      (map->Node {:state start :actions [] :path [start] :path-cost 0}))]
      (let [node (peek frontier)]
        (cond
          (empty? frontier) ; no solution
          nil

          (goal? problem (:state node))
          (dissoc node :state)

          :else
          (recur
            (conj explored (:state node))
            (reduce conj (pop frontier) (->> (actions problem (:state node))
                                             (map (partial child-node problem node))
                                             (remove #(explored (:state %)))))))))))
