(ns aoc.day11
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> input
       str/split-lines
       (map-indexed (fn [r line] (map-indexed (fn [c value] [[r c] (Character/digit ^char value 10)]) line)))
       (apply concat)
       (into {})))

(defn neighbours [[r c]]
  [[r (inc c)]
   [r (dec c)]
   [(inc r) (inc c)]
   [(dec r) (dec c)]
   [(inc r) (dec c)]
   [(dec r) (inc c)]
   [(inc r) c]
   [(dec r) c]])

(def flashed? zero?)
(defn ready-flash? [v] (> v 9))

(defn increment [grid] (reduce-kv (fn [acc k v] (assoc acc k (inc v))) {} grid))

(defn coordinates-where [f grid] (keep (fn [[k v]] (when (f v) k)) grid))
(defn coordinates-ready-to-flash [grid] (coordinates-where ready-flash? grid))
(defn coordinates-flashed [grid] (coordinates-where flashed? grid))

(defn flash-all [grid]
  (if-some [p (->> grid coordinates-ready-to-flash first)]
    (recur (->> (neighbours p)
                (filter #(contains? grid %))
                (remove (comp flashed? grid))
                (reduce (fn [acc k] (update acc k inc)) (assoc grid p 0))))
    grid))

(defn take-turn [grid] (-> grid increment flash-all))

(defn flash-counts [octopus-grid]
  (->> octopus-grid
       (iterate take-turn)
       (map (comp count coordinates-flashed))))

(defn -main []
  (let [octopus-grid (parse (slurp "resources/day11.txt"))
        part1 (->> octopus-grid flash-counts (take 101) (apply +))
        part2 (->> octopus-grid flash-counts (keep-indexed #(when (= 100 %2) %1)) first)]
    (run! println [part1 part2])))