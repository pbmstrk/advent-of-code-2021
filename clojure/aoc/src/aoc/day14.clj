(ns aoc.day14
  (:require [clojure.string :as str]))

(defn parse-chain [chain]
  (->> chain
       (partition 2 1)
       frequencies))

(defn parse-rules [rules]
  (->> rules
       str/split-lines
       (map #(str/split % #" -> "))
       (map (fn [[k v]] [(seq k) (first v)]))
       (into {})))

(defn parse [input]
  (let [[chain insertion-rules] (str/split input #"\n\n")]
    [(parse-chain chain) (parse-rules insertion-rules)]))

(defn update-chain [rules chain]
  (->> chain
       (reduce-kv (fn [acc [f s] v] (concat acc [{[f (rules [f s])] v} {[(rules [f s]) s] v}]))  {})
       (apply merge-with +)))

(defn count-chars [char-map]
  (reduce-kv (fn [acc [first-c _] v] (update acc first-c (fnil + 0) v)) {} char-map))

(defn char-diff-after-n [n input]
  (let [last-char (last (first (str/split-lines input)))
        [chain rules] (parse input)
        char-map  (last (take (inc n) (iterate (partial update-chain rules) chain)))
        final-count (count-chars char-map)
        adjusted-final (sort-by val (update final-count last-char + 1))
        min-char (first adjusted-final)
        max-char (last adjusted-final)]
    (- (val max-char) (val min-char))))

(defn -main []
  (let [input ( slurp "resources/day14.txt")
        part1 (->> input (char-diff-after-n 10))
        part2 (->> input (char-diff-after-n 40))]
    (run! println [part1 part2])))