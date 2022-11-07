(ns aoc.day12
  (:require [clojure.string :as str]))

(defn str->maps [line]
  (let [[start end] (str/split line #"-")]
    [{start [end]} {end [start]}]))

(defn parse [input]
  (->> input
       str/split-lines
       (mapcat str->maps)
       (apply merge-with into)))

(defn small-cave? [s] (every? #(Character/isLowerCase ^char %) s))

(defn valid-neighbour-a? [seen cave]
  (cond
    (= cave "start") false
    (= cave "end") true
    (and (small-cave? cave) (contains? seen cave)) false
    :else true))

(defn valid-neighbour-b? [seen cave]
  (cond
    (= cave "start") false
    (= cave "end") true
    (and (small-cave? cave) (some (partial <= 2) (vals seen)) (contains? seen cave)) false
    :else true))

(defn new-work-items [valid-neigh-f work-item graph]
  (let [[cave seen] work-item
        neighs (graph cave)]
    (->> neighs
         (filter (partial valid-neigh-f seen))
         (map (fn [c] [c (if (small-cave? c)
                           (update seen c #(inc (or % 0)))
                           seen)])))))

(defn num-paths [graph q work-queue-f]
  (loop [work-queue q
         num-paths 0]
    (cond
      (empty? work-queue) num-paths
      :else (let [work-item (peek work-queue)
                  remaining (pop work-queue)
                  [cave, _] work-item]
              (cond
                (= cave "end")  (recur remaining (inc num-paths))
                :else (let [new-work-queue (work-queue-f work-item graph)]
                        (recur (into remaining new-work-queue) num-paths)))))))

(defn -main []
  (let [graph (parse (slurp "resources/day12.txt"))
        part1 (num-paths graph [["start", {}]] (partial new-work-items valid-neighbour-a?))
        part2 (num-paths graph [["start", {}]] (partial new-work-items valid-neighbour-b?))]
    (run! println [part1 part2])))