(ns aoc.day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #" \| "))
       (map (fn [[s o]] [(str/split s #" ") (str/split o #" ")]))))

(defn signal-to-char-set [n signals]
  (->> signals
       (filter #(= (count %) n))
       first
       (into #{})))

(defn match-to-number [one-signal four-signal signal]
  (let [signal-length (count signal)
        char-set (->> signal (into #{}))
        sorted-string (->> signal sort (apply str))]
    (case signal-length
      2 [sorted-string \1]
      4 [sorted-string \4]
      3 [sorted-string \7]
      7 [sorted-string \8]
      6 (cond
          (= 1 (count (set/intersection char-set one-signal))) [sorted-string \6]
          (= 4 (count (set/intersection char-set four-signal))) [sorted-string \9]
          :else [sorted-string \0])
      5 (cond
          (= 2 (count (set/intersection char-set one-signal))) [sorted-string \3]
          (= 3 (count (set/intersection char-set four-signal))) [sorted-string \5]
          :else [sorted-string \2]))))

(defn get-mapping [signals]
  (let [one-signal (signal-to-char-set 2 signals)
        four-signal (signal-to-char-set 4 signals)]
    (->> signals
         (map #(match-to-number one-signal four-signal %))
         (into {}))))

(defn get-output-digits [[signals outputs]]
  (let [mapping (get-mapping signals)
        digit-list (map #(mapping (->> % sort (apply str))) outputs)]
    digit-list))

(defn -main []
  (let [input (parse (slurp "resources/day08.txt"))
        output-digits (map get-output-digits input)
        part1 (->> output-digits flatten (filter #{\1 \4 \7 \8}) count)
        part2 (->> output-digits (map #(Integer/parseInt (apply str %))) (apply +))]
    (run! println [part1 part2])))