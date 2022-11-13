(ns aoc.day22
  (:require [clojure.string :as str]))

(defrecord AxisRange [min-v max-v])
(defrecord Area3d [state x-range y-range z-range])

(defn intersect-range? [axis other]
  (let [{axis-min :min-v axis-max :max-v} axis
        {other-min :min-v other-max :max-v} other]
    (and (<= axis-min other-max) (>= axis-max other-min))))

(defn intersection-range [axis other]
  (let [{axis-min :min-v axis-max :max-v} axis
        {other-min :min-v other-max :max-v} other]
    (->AxisRange (max axis-min other-min) (min axis-max other-max))))

(defn intersect-area3d? [area other]
  (let [{area-x :x-range area-y :y-range area-z :z-range} area
        {other-x :x-range other-y :y-range other-z :z-range} other]
    (and (intersect-range? area-x other-x)
         (intersect-range? area-y other-y)
         (intersect-range? area-z other-z))))

(defn intersection-area3d [area other]
  (let [{area-x :x-range area-y :y-range area-z :z-range} area
        {other-x :x-range other-y :y-range other-z :z-range} other]
   (if (not (intersect-area3d? area other))
    nil
    (->Area3d
      (not (:state area))
      (intersection-range area-x other-x )
      (intersection-range area-y other-y )
      (intersection-range area-z other-z )))))

(defn axis-size [axis]
  (inc (- (:max-v axis) (:min-v axis))))

(defn volume [area]
  (* (if (:state area) 1 -1) (axis-size (:x-range area)) (axis-size (:y-range area)) (axis-size (:z-range area))))

(defn solve [instructions]
  (loop [ins instructions volumes []]
    (if (seq ins)
      (let [area (nth ins 0)
          overlaps (->> volumes (map #(intersection-area3d % area)) (remove nil?))
          new-volumes (if (not (empty? overlaps)) (concat volumes overlaps) volumes)]
        (if (:state area)
          (recur (rest ins) (conj new-volumes area))
          (recur (rest ins) new-volumes)))
      (apply + (map volume volumes)))))

(defn parse-line [line]
  (let [regex #"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)"
        [_ instruction & co-ords] (re-matches regex line)
        int-coords (map #(Integer/parseInt %) co-ords)
        [xmin xmax ymin ymax zmin zmax] int-coords
        bool-instruction (if (= instruction "on") true false)]
    (->Area3d bool-instruction (->AxisRange xmin xmax) (->AxisRange ymin ymax) (->AxisRange zmin zmax))))

(defn parse [input]
  (->> input
       str/split-lines
       (map parse-line)))

(def part1cube (->Area3d true (->AxisRange -50 50) (->AxisRange -50 50) (->AxisRange -50 50)))

(defn -main []
  (let [input (parse (slurp "resources/day22.txt"))
        filtered-input (filter #(intersect-area3d? % part1cube) input)
        part1 (solve filtered-input)
        part2 (solve input )]
    (run! println [part1 part2])))