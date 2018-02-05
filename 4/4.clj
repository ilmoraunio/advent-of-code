(defn valid-passphrase? [passphrase]
  (every? true? (map (fn [[_ frequency]] (= frequency 1)) (frequencies (clojure.string/split passphrase #"\s")))))

(def passphrases (comment "todo") [])
(->> passphrases (map valid-passphrase?) frequencies)
