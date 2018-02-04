;; determining the number of elements in an edge, given any value

(defn get-elements-in-edge [square]
  (let [sqrt (Math/sqrt square)]
    (cond
      (and (= (mod sqrt 1) 0.0) 
           (odd? (int sqrt)))           sqrt
      (= (mod (Math/floor sqrt) 2) 1.0) (+ (Math/floor sqrt) 2)
      :else                             (+ (Math/floor sqrt) 1))))
