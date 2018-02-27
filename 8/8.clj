(def input (slurp "8.txt"))
(def input-digested (clojure.string/split input #"\n"))

(def parsex #"(?<Register>\w+) (?<Operator>[\w<>=!]+) (?<Parameter>-?[0-9]+) if (?<Condition>(?<ConditionRegister>\w+) (?<ConditionOperator><|<=|==|!=|>=|>) (?<ConditionParameter>-?[0-9]+))")
(def transformation-map {:operator #(cond (= "inc" %) + 
                                          (= "dec" %) -)
                         :parameter #(Integer. %)
                         :condition-operator #(cond (= "<"    %) <
                                                    (= "<="   %) <=
                                                    (= "=="   %) =
                                                    (= "!="   %) not=
                                                    (= ">="   %) >=
                                                    (= ">"    %) >)
                         :condition-parameter #(Integer. %)})

(defn transform-map [transformation-map transformable-map]
  (reduce-kv (fn [m k v]
    (if (k transformation-map)
      (assoc m k ((k transformation-map) v))
      (assoc m k v))) (empty transformable-map) transformable-map))

(def rules-map (->> input-digested (map #(zipmap [:register           :operator
                                                  :parameter          :condition
                                                  :condition-register :condition-operator
                                                  :condition-parameter]
                                                 (rest (re-find parsex %))))
                                   (reduce (fn [a b] 
                                     (conj a (transform-map transformation-map b))) 
                                     (empty input-digested))))

(def register
  (apply hash-map (interleave (map :register rules-map) (repeat 0))))

(defn create-rule [{register-key :register :keys [operator
                                                  parameter
                                                  condition-register
                                                  condition-operator
                                                  condition-parameter] :as rule}
                   register]
  (let  [condition-register-value (get register condition-register)]
     (if (condition-operator condition-register-value condition-parameter)
       (update register register-key #(operator % parameter))
       register)))

(defn apply-business-rules [rules-map register]
 (loop [[rule & rst] rules-map
        register register]
  (if rule 
    (recur rst (create-rule rule register))
    register)))

(reduce (fn [[_ v1 :as m1] 
             [_ v2 :as m2]] 
             (if (> v1 v2) m1 m2)) (apply-business-rules rules-map register))

