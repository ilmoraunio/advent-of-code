(use 'clojure.test)

(def input (slurp "9.txt"))

(defn sanitize [input]
  (-> input
      (clojure.string/replace #"!." "")
      (clojure.string/replace #",?<.*?>" "")
      (clojure.string/replace #"\{," "{")
      (clojure.string/replace #",\}" "}")))

(testing "sanitize"
  (is (= (sanitize "<>")                  ""))
  (is (= (sanitize "<random characters>") ""))
  (is (= (sanitize "<<<<>")               ""))
  (is (= (sanitize "!!")                  ""))
  (is (= (sanitize "<{!>}>")              ""))
  (is (= (sanitize "<!!>")                ""))
  (is (= (sanitize "<!!!>>")              ""))
  (is (= (sanitize "<{o\"i!a,<{i<a>")     ""))
  (is (= (sanitize "{}")                  "{}"))
  (is (= (sanitize "{{}}")                "{{}}"))
  (is (= (sanitize "{{},{}}")             "{{},{}}"))
  (is (= (sanitize "{<>,<>}")             "{}"))
  (is (= (sanitize "{<>,{}}")             "{{}}"))

  (testing "smoke test"
    (is (nil? (re-find #"([^{},])" (sanitize input))))))

(defn inner-form [input]
  "See 9-inner-form.txt for example output"
  (-> input 
      (clojure.string/replace #"\{" "[")
      (clojure.string/replace #"\}" "]")
      read-string))

(defn calculate-score [inner-form]
  (loop [[group & groups :as form] inner-form
         depth-level 2
         breadth-counter (count inner-form)
         total-score (if (nil? inner-form) 0 1)]
    (cond
      (nil? group)
        total-score
      (and (empty? group) (empty? groups))
        (if (= breadth-counter 0)
          (+ total-score (inc depth-level))
          (+ total-score depth-level))
      :else
        (if (= breadth-counter 0)
          (recur (concat groups group) (inc depth-level) (count groups)        (+ total-score (inc depth-level)))
          (recur (concat groups group) depth-level       (dec breadth-counter) (+ total-score depth-level))))))


(testing "scoring points"
  (is (= (calculate-score [])          1))
  (is (= (calculate-score [[]])        3))
  (is (= (calculate-score [[] []])     5))
  (is (= (calculate-score [[[]]])      6))
  (is (= (calculate-score [[[]] []])   8))
  (is (= (calculate-score [[[]] [[]]]) 11)))

(calculate-score (inner-form (sanitize input)))
;; "Elapsed time: 32.204785 msecs"
;; 10050


;; part II
(time (reduce + (map (comp count second)
                     (re-seq #"<(.*?)>" (clojure.string/replace input #"!." "")))))
;; "Elapsed time: 0.914314 msecs"
;; 4482