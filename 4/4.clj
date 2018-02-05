(defn valid-passphrase? [passphrase]
  (every? true? (map (fn [[_ frequency]] (= frequency 1)) (frequencies (clojure.string/split passphrase #"\s")))))

(def passphrases (clojure.string/split (slurp "passphrases.txt") #"\n"))
(->> passphrases (map valid-passphrase?) frequencies)
