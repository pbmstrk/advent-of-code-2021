(ns aoc.day06
  (:require [clojure.string :as str]))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse [input]
  (->> (str/split input #",")
      (map str->int)))

(defn update-timer [timer]
  (if (= timer 0) 6 (dec timer)))

(defn update-day [fish-count]
  (let [day-0-fish (get fish-count 0 0)]
    (->
      (reduce-kv (fn [m k v] (update m (update-timer k) (fnil + 0) v)) {} fish-count)
      (assoc 8 day-0-fish))))

(defn nth-gen [n fish-count]
  (-> (iterate update-day fish-count)
      (nth n)))

(defn -main []
  (let [input (slurp "resources/day06.txt")
        initial-fish-count (->> input parse frequencies)
        part1 (->> initial-fish-count (nth-gen 80) vals (apply +))
        part2 (->> initial-fish-count (nth-gen 256) vals (apply +))]
    (run! println [part1 part2])))