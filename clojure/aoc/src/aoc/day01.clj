(ns aoc.day01
  (:require [clojure.string :as str]))

(defn str->int [i]
  (Integer/parseInt i))

(defn parse [input]
  (->> input str/split-lines (map str->int)))

(defn num-increases
  "Number of increases of the sums of n-element sliding windows."
  [n coll]
  (->> (map > (drop n coll) coll) (filter true?) count))

(defn -main []
  (let [measurements (parse (slurp "resources/day01.txt"))
        part1 (num-increases 1 measurements)
        part2 (num-increases 3 measurements)]
    (run! println [part1 part2])))