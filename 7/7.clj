(def input (slurp "7.txt"))

(def transformation-map
  {:weight #(Integer. %)
   :children #(when (not-empty %)
                (clojure.string/split % #"\h*,\h*"))})

(defn transform-map [transformation-map transformable-map]
  (reduce-kv (fn [m k v]
    (if (k transformation-map)
      (assoc m k ((k transformation-map) v))
      (assoc m k v))) (empty transformable-map) transformable-map))

(defn parse-tower-input [input]
  (let [re-seqs (re-seq #"(?<program>\w*) [(](?<weight>\d*)[)](?: -> )?(?<children>(\w+(?:,?\h*))*)?" input)]
    (->> re-seqs
         (map rest)
         (map #(zipmap [:program :weight :children] %))
         (reduce (fn [a b] (conj a (transform-map transformation-map b))) (empty re-seqs)))))

(parse-tower-input input)

;; 1. find :program where :children is nil
;; 2. find :program where step 1's :program is contained in :children
;;;; if found, repeat step 2
;;;; else return :program from previous step 2

;; playground

(def foo 
'({:program "cntj", :weight 57, :children nil}
  {:program "gyxo", :weight 61, :children nil} 
  {:program "ugml", :weight 68, :children ["gyxo" "ebii" "jptl"]} 
  {:program "jptl", :weight 61, :children nil} 
  {:program "tknk", :weight 41, :children ["ugml" "padx" "fwft"]}
  {:program "padx", :weight 45, :children ["pbga" "havc" "qoyq"]}
  {:program "qoyq", :weight 66, :children nil}
  {:program "fwft", :weight 72, :children ["ktlj" "cntj" "xhth"]}
  {:program "ktlj", :weight 57, :children nil}
  {:program "havc", :weight 66, :children nil}
  {:program "ebii", :weight 61, :children nil}
  {:program "xhth", :weight 57, :children nil}
  {:program "pbga", :weight 66, :children nil}))