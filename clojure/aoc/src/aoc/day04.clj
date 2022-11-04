(ns aoc.day04
  (:require [clojure.string :as str]))

(defrecord Game [numbers boards])

(defn split-blank-lines [input]
  (str/split input #"\n\n"))

(defn parse-numbers [numbers] (str/split numbers #","))

(defn parse-boards [bs]
  (let [board-squares (map str/split-lines bs)]
    (map #(map (fn [s] (str/split (str/trim s) #"\s+")) %) board-squares)))

(defn parse-game [input]
  (let [lines (split-blank-lines input)
        numbers (first lines)
        boards (rest lines)]
    (->Game (parse-numbers numbers) (parse-boards boards))))

(defn transpose [board] (apply map vector board))

(defn axis-win? [row] (every? #(= :crossed %) row))

(defn board-win? [board]
  (or (some axis-win? board)
      (some axis-win? (transpose board))))

(defn mark-row [row value]
  (map #(if (= % value) :crossed %) row))

(defn mark-board [board value]
  (map #(mark-row % value) board))

(defn mark-boards [boards value]
  (map #(mark-board % value) boards))

(defn extend-won-boards [b new-boards x]
  (concat b (map vector new-boards (repeat x))))

(defn partition-by-winning [boards]
  ((juxt filter remove) board-win? boards))

(defn play [gs]
  (loop [boards (:boards gs)
         numbers (:numbers gs)
         won-boards []]
    (let [[x & xs] numbers
          crossed-boards (mark-boards boards x)
          [winning-boards not-winning-boards] (partition-by-winning crossed-boards)
          updated-won-boards (extend-won-boards won-boards winning-boards x)]
      (if (empty? not-winning-boards)
        updated-won-boards
        (recur not-winning-boards xs updated-won-boards)))))

(defn sum-uncrossed-board [board]
  (apply + (for [row board x row :when (not= x :crossed)] (Integer/parseInt x))))

(defn -main []
  (let [game (parse-game (slurp "resources/day04.txt"))
        won-boards-ordered (play game)
        [f-board f-num] (first won-boards-ordered)
        [l-board l-num] (last won-boards-ordered)
        part1 (* (sum-uncrossed-board f-board) (Integer/parseInt f-num))
        part2 (* (sum-uncrossed-board l-board) (Integer/parseInt l-num))]
    (run! println [part1 part2])))