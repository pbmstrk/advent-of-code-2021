(ns aoc.day18
  (:require [clojure.string :as str]
            [clojure.zip :as z]))

(defn parse [input]
  (->> input str/split-lines (mapv read-string)))

(def regular? (comp int? z/node))
(def pair? (comp vector? z/node))
(def can-split? (fn [z] (and (regular? z) (> (z/node z) 9))))
(defn can-explode? [z] (and (pair? z) (= (count (z/path z)) 4)))

(defn find-node-forwards [pred zipper]
  (loop [z zipper]
    (cond
      (z/end? z) nil
      (pred z) z
      :else (recur (z/next z)))))

(defn find-node-backwards [pred zipper]
  (loop [z zipper]
    (cond
      (nil? z) nil
      (pred z) z
      :else (recur (z/prev z)))))

(defn next-regular [zipper]
  (find-node-forwards regular? (z/next zipper)))

(defn prev-regular [zipper]
  (find-node-backwards regular? (z/prev zipper)))

(defn go-up [zipper]
  (z/vector-zip (z/root zipper)))

(defn explode [zipper]
  (let [explode (find-node-forwards can-explode? zipper)]
    (if (not (nil? explode))
      (let [[l r] (z/node explode)
            new-zip1 (z/replace explode 0)
            possible-left (prev-regular new-zip1)
            new-zip2 (if (not (nil? possible-left))
                       (next-regular (z/edit possible-left + l))
                       new-zip1)
            possible-right (next-regular new-zip2)
            new-zip3 (if (not (nil? possible-right))
                       (prev-regular (z/edit possible-right + r))
                       new-zip2)]
        new-zip3)
      (go-up zipper))))

(defn split [zipper]
  (let [split (find-node-forwards can-split? zipper)]
    (if (not (nil? split))
      (z/edit split (fn [x] [(int (/ x 2)) (int (Math/ceil (/ x 2)))]))
      (go-up zipper))))

(defn reduce-fish [zipper]
  (let [exp (explode zipper)]
    (if (not= exp zipper)
      (recur exp)
      (let [spl (split zipper)]
        (if (not= spl zipper)
          (recur spl)
          (z/root zipper))))))

(defn- magnitude [x]
  (if (vector? x)
    (+ (* 3 (magnitude (first  x)))
       (* 2 (magnitude (second x))))
    x))

(defn -main []
  (let [prep-fish (comp reduce-fish z/vector-zip vector)
        snailfish (-> (slurp "resources/day18.txt") parse)
        part1 (magnitude (reduce prep-fish snailfish))
        part2 (apply max (for [x snailfish
                               y snailfish]
                           (magnitude (prep-fish x y))))]
        (run! println [part1 part2])))



