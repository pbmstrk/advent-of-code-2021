(ns aoc.day05
  (:require [clojure.string :as str]))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse-points [line]
  (let [[_ & points] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
    (->> points
         (map str->int)
         (partition 2))))

(defn parse [input]
  (map parse-points (str/split-lines input)))

(defn is-straight-line? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn new-range [a b]
  (if (<= a b) (range a (inc b)) (range a (dec b) -1)))

(defn to-points [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (for [y (new-range y1 y2)] [x1 y])
    (= y1 y2) (for [x (new-range x1 x2)] [x y1])
    :else (map vector (new-range x1 x2) (new-range y1 y2))))

(defn count-overlaps [lines]
  (->> lines
       (mapcat to-points)
       frequencies
       (filter #(> (second %) 1))
       vals
       count))

(defn -main []
  (let [lines (parse (slurp "resources/day05.txt"))
        part1 (->> lines (filter is-straight-line?) count-overlaps)
        part2 (->> lines count-overlaps)]
    (run! println [part1 part2])))