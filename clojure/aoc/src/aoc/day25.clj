(ns aoc.day25
  (:require [clojure.string :as str]))

(defn list-points [input]
  (for [[r line] (map-indexed vector input)
        [c value] (map-indexed vector line)]
    [value [r c]]))

(defn parse [input]
  (let [m (->> input str/split-lines list-points)
        points (map second m)
        num-rows (apply max (map first points))
        num-cols (apply max (map second points))]
    (reduce
      (fn [acc [dir point]]
        (case dir
          \> (update-in acc [:right] conj point)
          \v (update-in acc [:down] conj point)
          \. acc)) {:right #{}, :down #{}, :num-rows (inc num-rows), :num-cols (inc num-cols)} m)))

(defn move [moving blocking update-f]
  (let [new-points (reduce (fn [s p]
                             (cond
                               ((some-fn moving blocking) (update-f p)) s
                               :else (-> s (disj! p) (conj! (update-f p))))) (transient moving) moving)]
    (persistent! new-points)))

(defn update-right [num [r c]]
  [r (mod (inc c) num)])

(defn update-down [num [r c]]
  [(mod (inc r) num) c])

(defn move-right [m]
  (assoc m :right
           (move (:right m) (:down m) (partial update-right (:num-cols m)))))

(defn move-down [m]
  (assoc m :down
           (move (:down m) (:right m) (partial update-down (:num-rows m)))))

(defn turn [m]
  (-> m
      move-right
      move-down))

(defn solve [m]
  (loop [cur-map m counter 1]
    (let [new-map (turn cur-map)]
      (cond
        (= cur-map new-map) counter
        :else (recur new-map (inc counter))))))

(defn -main []
  (let [m (parse (slurp "resources/day25.txt"))
        part1 (solve m)]
     (println part1)))
