(ns aoc.day15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [input]
       (->> input
            str/split-lines
            (map-indexed
              (fn [r row]
                (map-indexed
                  (fn [c value] [[r c] (Character/digit ^char value 10)]) row)))
            (apply concat)
            (into {})))

(defn valid? [grid p]
       (contains? grid p))

(defn get-neighbours [grid [r c]]
       (->> [[(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]]
            (filter #(valid? grid %))))

(defn cost [grid _ f]
       (grid f))

(defn dijkstra [neighbour-f cost-f init-state target]
       (let [q (priority-map init-state 0)
             v #{}]
         (loop [queue q visited v]
           (if (empty? queue)
             (throw (Exception. "Target not found"))
             (let [[current-vertex current-distance] (peek queue)
                   options (->> current-vertex
                                neighbour-f
                                (remove visited)
                                (map #(vector % (+ current-distance (cost-f current-vertex %))))
                                (into {}))]
               (if (= current-vertex target)
                 current-distance
                 (recur (merge-with min (pop queue) options) (conj visited current-vertex))))))))

(defn new-value [val]
       (cond
         (> val 9) (+ 1 (mod val 10))
         :else val))

(defn expand-grid [grid]
       (let [expand-coords (for [r [0 100 200 300 400]
                                 c [0 100 200 300 400]]
                             [[r c] (quot (+ r c) 100)])]
         (persistent! (reduce-kv (fn [grid [or oc] v]
                      (reduce (fn [acc [[r c] add]] (assoc! acc [(+ or r) (+ oc c)] (new-value (+ v add)))) grid expand-coords)) (transient {}) grid))))

(defn target [grid]
       (apply max-key #(apply + %) (keys grid)))

(defn -main []
  (let [grid (parse (slurp "resources/day15.txt"))
        expanded-grid  (expand-grid grid)
        part1 (dijkstra
                (partial get-neighbours grid)
                (partial cost grid)
                [0 0]
                (target grid))
        part2   (dijkstra
                (partial get-neighbours expanded-grid)
                (partial cost expanded-grid)
                [0 0]
                (target expanded-grid))]
    (run! println [part1 part2])))