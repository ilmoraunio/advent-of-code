;; Let's play a game of automated kalaha!

(defn difference-vector [v]
  (loop [diff-v (mapv (fn [_] constantly 0) v)
         n (apply max v)
         idx (drop (inc (.indexOf v n)) (cycle (range 0 (count v))))]
    (if (zero? n)
      diff-v
      (recur (update diff-v (first idx) inc) (dec n) (rest idx)))))

(defn make-new-state [v]
  (mapv + (assoc v (.indexOf v (apply max v)) 0)
          (difference-vector v)))

(defn play-until-loops [v]
  (loop [states #{}
         new-state (make-new-state v)]
    (if (states new-state)
      (inc (count states))
      (recur (conj states new-state) (make-new-state new-state)))))

;; user=> (time (play-until-loops [14 0 15  12  11  11  3 5 1 6 8 4 9 1 8 4]))
;; "Elapsed time: 360.983068 msecs"
;; 11137

;; user=> (time (play-until-loops [14 0 15  12  11  11  3 5 1 6 8 4 9 1 8 4 5 20 4 5 1 0 200 4 3 20 5 6 8000 500000 20 100 20]))
;; "Elapsed time: 13074.416489 msecs"
;; 849