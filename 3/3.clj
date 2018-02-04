(defn edge-count [square]
  (int (let [sqrt (Math/sqrt square)]
         (cond
           (and (= (mod sqrt 1) 0.0) 
                (odd? (int sqrt)))           sqrt
           (= (mod (Math/floor sqrt) 2) 1.0) (+ (Math/floor sqrt) 2)
           :else                             (+ (Math/floor sqrt) 1)))))

(defn center-line-squares [square]
  (let [step              (dec (edge-count square))
        prefix            (dec (/ (dec (edge-count square)) 2))
        start-square      (int (inc (Math/pow (- (edge-count square) 2) 2)))]
    [(+ prefix start-square)
     (+ prefix start-square step)
     (+ prefix start-square step step)
     (+ prefix start-square step step step)]))

(defn distance-to-center-line [square]
  ;; determine the elements-in-edge
  ;; calculate the center line square values for that layer (ie. [2, 4, 6, 8] when edge-count is 3)
  ;; compare square against center line values and determine nearest candidate
  ;; calculate steps
  (let [distances (->> (center-line-squares square)
                       (map #(- % square))
                       (map #(Math/abs %)))]
    ;; Some maximum values in distances will not reflect the truth
    ;; (for instance a 10 has only 3 steps to 23, but is shown as 13)
    ;; luckily we only care about the minimum value, which means we can
    ;; discard the rest of values in distances.
    (reduce min distances)))

(defn distance-from-center-line-to-square-one [square]
  (let [n (edge-count square)]
    (/ (dec n) 2)))

(defn manhattan-distance-of-spiral-memory [square]
  (+ (distance-to-center-line square) (distance-from-center-line-to-square-one square)))
