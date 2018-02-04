;; determining the number of elements in an edge, given any value

(defn get-elements-in-edge [square]
  (int (let [sqrt (Math/sqrt square)]
         (cond
           (and (= (mod sqrt 1) 0.0) 
                (odd? (int sqrt)))           sqrt
           (= (mod (Math/floor sqrt) 2) 1.0) (+ (Math/floor sqrt) 2)
           :else                             (+ (Math/floor sqrt) 1)))))

(defn get-distance-to-center-line [square]
  ;; determine the elements-in-edge
  ;; calculate the center line square values for that layer (ie. [2, 4, 6, 8] when get-elements-in-edge is 3)
  ;; compare square against center line values and determine nearest candidate
  ;; calculate steps
  (let [n (get-elements-in-edge square)]
    ))

(defn get-distance-from-center-line-to-square-one [square]
  ;; determine the elements-in-edge
  ;; subtract elements-in-edge by one and divide by 2 -> how many steps it takes to get to square value 1
  (let [n (get-elements-in-edge square)]
    (/ (dec n) 2)))
