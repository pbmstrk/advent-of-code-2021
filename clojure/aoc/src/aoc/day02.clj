(ns aoc.day02
  (:require [clojure.java.io]
            [clojure.string :as str]))

(defrecord Position [horizontal depth])
(defrecord PositionWithAim [horizontal depth aim])

(defn vec->command [[dir mag]]
  {:direction dir :magnitude (Integer/parseInt mag)})

(defn parse [input]
  (->> input
       str/split-lines
       (map #(clojure.string/split % #" "))
       (map vec->command)))

(defn update-position [position instruction]
  (let [{horizontal :horizontal depth :depth} position,
        {direction :direction magnitude :magnitude} instruction]
    (case direction
      "forward" (assoc position :horizontal (+ horizontal magnitude))
      "down" (assoc position :depth (+ depth magnitude))
      "up" (assoc position :depth (- depth magnitude)))))

(defn update-position-with-aim [position instruction]
  (let [{horizontal :horizontal depth :depth aim :aim} position,
        {direction :direction magnitude :magnitude} instruction]
    (case direction
      "forward" (->PositionWithAim (+ horizontal magnitude) (+ depth (* aim magnitude)) aim)
      "down" (assoc position :aim (+ aim magnitude))
      "up" (assoc position :aim (- aim magnitude)))))

(defn get-result [final-state]
  (let [{horizontal :horizontal depth :depth} final-state]
    (* horizontal depth)))

(defn -main []
  (let [commands (parse (slurp "resources/day02.txt"))
        final-position (reduce update-position (->Position 0 0) commands)
        final-position-with-aim (reduce update-position-with-aim (->PositionWithAim 0 0 0) commands)]
    (run! println (map get-result [final-position final-position-with-aim]))))