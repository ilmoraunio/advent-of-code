(defn valid-passphrase? [passphrase]
  (every? true? (map (fn [[_ frequency]] (= frequency 1)) (frequencies (clojure.string/split passphrase #"\s")))))

(def passphrases (clojure.string/split (slurp "passphrases.txt") #"\n"))
(->> passphrases (map valid-passphrase?) frequencies)

;; part II

(defn valid-passphrase-part-II? [passphrase]
  (every? true? (map (fn [[_ frequency]] (= frequency 1)) 
                     (frequencies (map frequencies (clojure.string/split passphrase #"\s"))))))

(->> passphrases (map valid-passphrase-part-II?) frequencies)
