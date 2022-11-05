(ns aoc.day10
  (:require [clojure.java.io]
            [clojure.string :as str]))

(def matching {\) \(, \] \[, \} \{, \> \<})

(def char-score {\) 3, \] 57, \} 1197, \> 25137})

(defn parse [input]
  (->> input
       str/split-lines
       (map vec)))

(defn is-closing? [c] (contains? matching c))

(defn reduce-char-list [char-list]
  (reduce (fn [stack c]
            (cond
              (empty? stack) (if (is-closing? c) (reduced c) [c])
              :else (let [is-opening (not (is-closing? c))
                          [top rest] ((juxt peek pop) stack)]
                      (cond
                        is-opening (conj stack c)
                        :else (if (= (matching c) top) rest (reduced c)))))) [] char-list))

(defn score-remaining [char-list]
  (reduce (fn [acc c] (case c
                        \( (+ (* 5 acc) 1)
                        \[ (+ (* 5 acc) 2)
                        \{ (+ (* 5 acc) 3)
                        \< (+ (* 5 acc) 4))) 0 char-list))

(defn middle-element [l]
  (nth l  (quot (count l) 2)))

(defn -main []
  (let [input (parse (slurp "resources/day10.txt"))
        results (map reduce-char-list input)
        part1 (->> results (filter char?) (map char-score) (apply +))
        part2 (->> results (filter coll?) (map (comp score-remaining reverse)) sort middle-element)]
    (run! println [part1 part2])))