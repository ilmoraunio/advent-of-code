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

(defn find-root-node [programs]
  (let [haystack (filter :children programs)]
    (loop [root-node-candidate (first (remove :children programs))
           [{:keys [program children] :as haystraw} & rst] haystack]
      (if (some #{(:program root-node-candidate)} children)
        (recur haystraw haystack)
        (if (empty? rst)
          root-node-candidate
          (recur root-node-candidate rst))))))

(time (find-root-node (parse-tower-input input)))
;; "Elapsed time: 13.455274 msecs"
;; {:program "svugo", :weight 32, :children ["xolvnpy" "gjxqx" "gtzxxav" "njorjq" "qpiklvf"]}

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