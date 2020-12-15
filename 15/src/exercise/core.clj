(ns exercise.core
  (:gen-class))

(defn generate-index-map [numbers idx]
  (if (empty? numbers)
    {}
    (assoc (generate-index-map (rest numbers) (inc idx)) (first numbers) (list idx))
    )
  )

(defn speak-number
  "Given a list numbers generate numbers according to the following rules:
  if last number is being spoken for the first time write 0
  if number has been spoken before that find most recent two turns this number was spoken and speak their difference

  returns number spoken at limit"
  [starting-numbers max-limit]
  (loop [index-map (generate-index-map starting-numbers 0)
         previous-number (nth starting-numbers (dec (count starting-numbers)))
         turn (count starting-numbers)]
      (if (>= turn max-limit)
        previous-number
        (let [matching-indices (get index-map previous-number [])]
          (if (= 0 (mod turn 1000000))
            (println "Turn:" turn)
            :default
            )
          (let [speak-number (if (<= (count matching-indices) 1) 0
                               (- (nth matching-indices 1) (nth matching-indices 0)))
                new-data (conj (into [] (get index-map speak-number)) turn)
                trunc-data (if (> (count new-data) 2) (drop (- (count new-data) 2) new-data) new-data)]
            (recur (assoc index-map speak-number trunc-data) speak-number (inc turn))
          )
        )
      )
    )
  )

(defn -main
  "Advent of Code. Day 15"
  [&  args]
    (println "Solution of part 1" (speak-number [1 20 11 6 12 0] 2020))
    (println "WARNING! This may take a while... Get that coffee onto the stovetop.")
    (println "Solution of part 2" (speak-number [1 20 11 6 12 0] 30000000))
  )
