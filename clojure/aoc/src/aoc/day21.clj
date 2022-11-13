(ns aoc.day21)

(defrecord Player-State [position score])
(defrecord Game-State [player-1 player-2 turn])

(defn play-turn [roll-sum game-state]
  (let [{player1-position :position  player1-score :score} (:player-1 game-state)
        {player2-position :position  player2-score :score} (:player-2 game-state)
        turn (:turn game-state)]
    (cond
      (= 0 turn) (let [intermediate-position (mod (+ player1-position roll-sum) 10)
                               new-position (if (= 0 intermediate-position) 10 intermediate-position)]
                           (->Game-State (->Player-State new-position (+ player1-score new-position)) (:player-2 game-state) (mod (inc turn) 2)))
      :else (let [intermediate-position (mod (+ player2-position roll-sum) 10)
                  new-position (if (= 0 intermediate-position) 10 intermediate-position)]
              (->Game-State (:player-1 game-state) (->Player-State new-position (+ player2-score new-position)) (mod (inc turn) 2))))))

(defn won? [state player threshold]
  (>= (get-in state [player :score]) threshold))

(defn play-game [game-state]
  (let [all-rolls (for [i (range)]
                    (+ (+ 1 (* i 3)) (+ 2 (* i 3)) (+ 3 (* i 3))))]
    (loop [state game-state [roll & rolls] all-rolls c 0]
      (cond
        (won? state :player-1 1000) (* c (get-in state [:player-2 :score]))
        (won? state :player-2 1000) (* c (get-in state [:player-1 :score]))
        :else (recur (play-turn roll state) rolls (+ c 3))))))

(declare play-roll)

(defn play-nd-game [state cache]
  (let [all-roll (for [x (range 1 4)
                       y (range 1 4)
                       z (range 1 4)]
                   (+ x y z))]
    (cond
      (won? state :player-1 21) [cache [1 0]]
      (won? state :player-2 21) [cache [0 1]]
      :else (if (contains? cache state)
              [cache (cache state)]
              (let [sub-games (map (fn [r] (play-turn r state)) all-roll)
                    [cache' result] (reduce play-roll [cache [0 0]] sub-games)]
                [(assoc cache' state result) result])))))

(defn play-roll [[cache [r1 r2]] gs]
  (let [[c' [a' b']] (play-nd-game gs cache)]
    [c' [(+ r1 a') (+ r2 b')]]))

(defn -main []
  (let [part1 (play-game (->Game-State (->Player-State 10 0) (->Player-State 1 0) 0))
        [_ wins] (play-nd-game (->Game-State (->Player-State 10 0) (->Player-State 1 0) 0) {})
        part2 (apply max wins)]
    (run! println [part1 part2])))