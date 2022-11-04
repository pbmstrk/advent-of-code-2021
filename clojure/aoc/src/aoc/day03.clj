(ns aoc.day03
  (:require [clojure.string :as str]))

(defn parse [input] (str/split-lines input))

(defn transpose [vec] (apply mapv vector vec))

(defn str->int [s] (Integer/parseInt s 2))

(defn most-common [s]
  (let [freq (frequencies s)]
    (if (>= (get freq \1 0) (get freq \0 0)) \1 \0)))

(defn least-common [s]
  (let [freq (frequencies s)]
    (if (< (get freq \1 0) (get freq \0 0)) \1 \0)))

(defn single-pass-rate-calculator [input comp-f]
  (->> input
       transpose
       (map comp-f)
       (apply str)
       str->int))

(defn get-gamma-rate [input] (single-pass-rate-calculator input most-common))
(defn get-epsilon-rate [input] (single-pass-rate-calculator input least-common))

(defn power-consumption [input]
  (let [g (get-gamma-rate input)
        e (get-epsilon-rate input)]
    (* g e)))

(defn multi-pass-rate-calculator [input comp-f]
  (loop [rows input index 0]
    (if (= 1 (count rows))
      (str->int (first rows))
      (let [columns (transpose rows)
            cur-column (columns index)
            value (comp-f cur-column)]
        (recur (filter #(= (nth % index) value) rows) (inc index))))))

(defn get-oxygen-rating [input] (multi-pass-rate-calculator input most-common))
(defn get-co2-rating [input] (multi-pass-rate-calculator input least-common))

(defn life-support-rating [input]
  (let [o (get-oxygen-rating input)
        c (get-co2-rating input)]
    (* o c)))

(defn -main []
  (let [numbers (parse (slurp "resources/day03.txt"))
        pc (power-consumption numbers)
        ls (life-support-rating numbers)]
    (run! println [pc ls])))