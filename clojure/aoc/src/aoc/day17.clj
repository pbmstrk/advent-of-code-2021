(ns aoc.day17)

(defrecord State [x y x-velocity y-velocity])
(defrecord Target [x-min x-max y-min y-max])

(defn hit-target? [target state]
    (and (>= (:x state) (:x-min target))
         (<= (:x state) (:x-max target))
         (>= (:y state) (:y-min target))
         (<= (:y state) (:y-max target))))

(defn overshot? [target state]
  (or (> (:x state) (:x-max target))
      (< (:y state) (:y-min target))))

(defn update-state [state]
  (->State
    (+ (:x state) (:x-velocity state))
    (+ (:y state) (:y-velocity state))
    (cond
      (< (:x-velocity state) 0) (inc (:x-velocity state))
      (> (:x-velocity state) 0) (dec (:x-velocity state))
      :else 0)
    (dec (:y-velocity state))))

(defn run-simulation [x-v y-v target]
  (let [initial-state (->State 0 0 x-v y-v)]
    (loop [n 0 state initial-state y-max Integer/MIN_VALUE]
      (cond
        (> n 1000) nil
        (overshot? target state) nil
        (hit-target? target state) y-max
        :else (let [new-state (update-state state)]
                (recur (inc n) new-state (max y-max (:y new-state))))))))

(defn -main []
  (let [target (->Target 85 145 -163 -108)
        all-heights (for [x-v (range -200 200)
                          y-v (range -200 200)]
                   (run-simulation x-v y-v target))
        heights (remove nil? all-heights)
        part1 (apply max heights)
        part2 (count heights)]
  (run! println [part1 part2])))
