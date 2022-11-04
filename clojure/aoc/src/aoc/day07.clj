(ns aoc.day07
  (:require [clojure.string :as str]))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse [input]
  (->> (str/split input #",")
       (map str->int)))

(defn compute-distance-from-point [f point numbers]
  (map (fn [n] (f (abs (- n point)))) numbers))

(defn sum [numbers]
  (apply + numbers))

(defn sum-up-to [n] (* n (/ (+ n 1) 2)))

(defn find-minimum-with [f numbers]
  (let [[min-num max-num] (apply (juxt min max) numbers)
        distances (map #(compute-distance-from-point f % numbers) (range min-num max-num))]
    (->> distances
         (map sum)
         (apply min))))

(defn -main []
  (let [subs (-> (slurp "resources/day07.txt") parse)
        part1 (find-minimum-with identity subs)
        part2 (find-minimum-with sum-up-to subs)]
    (run! println [part1 part2])))
