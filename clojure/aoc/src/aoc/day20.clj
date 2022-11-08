(ns aoc.day20
  (:require [clojure.string :as str]))

(defn parse-pixel [c]
  (if (= c \.) \0 \1))

(defn parse [input]
  (let [[algorithm _ & image-string] (str/split-lines input)
        image-vec (mapv (fn [s] (mapv (fn [c] (parse-pixel c)) s)) image-string)]
    [(mapv parse-pixel algorithm) image-vec]))

(defn get-default-value [iteration]
  (if (= 0 (mod iteration 2)) \0 \1))

(defn get-window [[r c]]
  [[(dec r) (dec c)] [(dec r) c] [(dec r) (inc c)]
   [r (dec c)] [r c] [r (inc c)]
   [(inc r) (dec c)] [(inc r) c] [(inc r) (inc c)]])

(defn get-value [image value [r c]]
  (get-in image [r c] value))

(defn bin->dec [binary-string]
  (Integer/parseInt binary-string 2))

(defn next-round [image alg value]
  (mapv (fn [r] (mapv (fn [c]
                        (->> [r c]
                             get-window
                             (map #(get-value image value %))
                             (apply str)
                             bin->dec
                             alg)) (range -1 (-> image first count inc)))) (range -1 (-> image count inc))))

(defn simulate [starting-image alg rounds]
  (reduce (fn [image iteration] (next-round image alg (get-default-value iteration))) starting-image (range 0 rounds)))

(defn count-pixels [image]
    (->> (flatten image)
         (filter #(= \1 %))
         count))

(defn -main []
  (let [[alg image] (parse (slurp "resources/day20.txt"))
        part1 (-> (simulate image alg 2) count-pixels)
        part2 (-> (simulate image alg 50) count-pixels)]
    (run! println [part1 part2])))