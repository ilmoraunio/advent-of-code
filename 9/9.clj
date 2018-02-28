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
  (-> input 
      (clojure.string/replace #"\{" "[")
      (clojure.string/replace #"\}" "]")
      read-string))

;; (inner-form (sanitize input))

(loop [[group & rst :as form] (inner-form (sanitize input))
       depth-level 1
       total-score 0]
  (prn "group" (clojure.pprint/pprint group))
  (prn "rst" (clojure.pprint/pprint rst))
  )