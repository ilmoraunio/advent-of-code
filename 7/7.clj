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


;; part II
;; the right solution is 1152 (program name 'sphbbz')
;; still todo: calculate the difference and subtract from 'sphbbz's :weight

(defn find-node [input program-name]
  (first (filter #(->> % :program (= program-name)) input)))

(defn disc-weights [input {:keys [children] :as program}]
  (if children
    (for [child (map (partial find-node input) children)]
      (let [v (cons (:weight child) (disc-weights input child))
            reduced-v (apply + (flatten v))]
        (when (and (not-empty (rest v))
                 (apply not= (rest v)))
          (prn "These children / this node is out of balance" children child (rest v)))
        reduced-v))
    '()))

(time (disc-weights (parse-tower-input input) (find-root-node (parse-tower-input input))))
;; "Elapsed time: 18.11239 msecs"
;; "These children / this node is out of balance" ["yruivis" "rizjob" "qsfwl" "asckjlv" "sfqwrge" "bncdhrm"] {:program "yruivis", :weight 2760, :children ["oxipms" "ggpau" "sphbbz"]} (2671 2671 2680)
;; "These children / this node is out of balance" ["xolvnpy" "gjxqx" "gtzxxav" "njorjq" "qpiklvf"] {:program "gjxqx", :weight 14, :children ["yruivis" "rizjob" "qsfwl" "asckjlv" "sfqwrge" "bncdhrm"]} (10782 10773 10773 10773 10773 10773)
;; (64652 64661 64652 64652 64652)