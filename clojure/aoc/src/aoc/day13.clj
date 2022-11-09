(ns aoc.day13
  (:require [clojure.string :as str]))

;; note the switch here
(defn str->co-ord [coord]
  (let [[c r] (str/split coord #",")]
    [(Integer/parseInt r) (Integer/parseInt c)]))

(defn parse-instruction [[axis, val]]
  (cond
    (= axis "x") ["X", (Integer/parseInt val)]
    (= axis "y") ["Y", (Integer/parseInt val)]))

(defn parse [input]
  (let [[co-ords instructions] (str/split input #"\n\n")]
    [(map str->co-ord (str/split-lines co-ords)) (map (comp parse-instruction #(str/split % #"=") last #(str/split % #" ")) (str/split-lines instructions))]))

(defn update-coords [points [axis n]]
  (cond
    (= axis "Y") (let [points-not-on-fold (filter (fn [[r _]] (not= r n)) points)]
                   (map (fn [[r c]] (if
                                      (< r n)
                                      [r c]
                                      [(- n (- r n)) c])) points-not-on-fold))
    (= axis "X") (let [points-not-on-fold (filter (fn [[_ c]] (not= c n)) points)]
                   (map (fn [[r c]] (if
                                      (< c n)
                                      [r c]
                                      [r (- n (- c n))])) points-not-on-fold))))

(defn format-dots [dots]
  (let [min-r (apply min (map first dots))
        min-c (apply min (map second dots))
        max-r (apply max (map first dots))
        max-c (apply max (map second dots))]
    (mapv (fn [r]
           (mapv (fn [c]
                  (if (some #{[r c]} dots) \# \space))
                (range min-c (inc max-c)))) (range min-r (inc max-r)))))

(defn -main []
  (let [[points instructions] (parse (slurp "resources/day13.txt"))
        part1  (->> (first instructions) (update-coords points) distinct count)
        part2 (->> (reduce update-coords points instructions ) format-dots)]
    (println part1)
    (run! (comp println (partial str/join " ")) part2)))