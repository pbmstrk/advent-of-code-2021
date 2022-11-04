(ns aoc.day09
  (:require [clojure.string :as str]))

(defn parse-gridlines-with [f grid]
  (->> grid
       (map-indexed (fn [r row] (map-indexed (fn [c value] [[r c] (f value)]) row)))))

(defn parse [input]
  (->> input
       str/split-lines
       (parse-gridlines-with #(Character/digit ^char % 10))
       (apply concat)
       (into {})))

(defn is-valid? [grid, p]
  (if (nil? (grid p)) false true))

(defn neighbours [grid [r c]]
  (->> [[r (inc c)] [r (dec c)] [(inc r) c] [(dec r) c]]
       (filter #(is-valid? grid %))))

(defn get-value [grid p]
  (get grid p))

(defn is-low-point? [grid p]
  (->> (neighbours grid p)
       (map #(get-value grid %))
       (every? #(> % (get-value grid p)))))

(defn risk-level [grid p]
  (+ 1 (get-value grid p)))

(defn expand-basin [grid p]
  (loop [seen #{} todo [p]]
    (cond
      (empty? todo) seen
      :else (let [new-seen (into seen todo)
                  neighs (->> todo
                              (mapcat #(neighbours grid %))
                              (into #{}))
                  new-todo (filter (fn [p] (and (< (get-value grid p) 9) (not (contains? seen p)))) neighs)]
              (recur new-seen new-todo)))))

(defn -main []
  (let [grid (-> (slurp "resources/day09.txt") parse)
        low-points (filter #(is-low-point? grid %) (keys grid))
        basin-sizes (map #(count (expand-basin grid %)) low-points)
        part1  (apply + (map #(risk-level grid %) low-points))
        part2 (->> basin-sizes sort reverse (take 3) (apply *))]
    (run! println [part1 part2])))