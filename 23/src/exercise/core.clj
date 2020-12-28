(ns exercise.core
  (:gen-class))

(defn split-right
  "gets element at idx mod (count in )"
  [in idx]
  (let [sz (count in)
        n1 (nth in (mod idx sz))
        n2 (nth in (mod (inc idx) sz))
        n3 (nth in (mod (inc (inc idx)) sz))
        remaining (filter #(and (not= % n1) (not= % n2) (not= % n3)) in)]
    [(into [] remaining) [n1 n2 n3]]
  )
)

(defn find-label
  "Crab selects a label that is at "
  [in val]
  (let [destination-label val
        ]
    ; (println "dest-label" in destination-label)
    (loop [label destination-label]
      (if (= true (some #(= % label) in))
        label
        (if (< label (apply min in))
          (apply max in)
          (recur (dec label))
          )
      )
    )
  )
)

(defn rotate-until [in idx el]
  ; rotate until el is at index in list
  (loop [lst (into [] in)]
    (if (= (nth lst idx) el)
      lst
      (recur (into [] (concat [(peek lst)] (into [] (drop-last lst))))
        )
      )
    )
  )

(defn simulate-part1 [in steps]
  (loop [state in
         idx 0
         step 0]
    (if (>= step steps)
      state
      (let [[remaining el3] (split-right state (inc idx))
            destination-label (find-label remaining (dec (nth state (mod idx (count state)))))
            [split-left split-right] (split-at (inc (.indexOf remaining destination-label)) remaining)
            logical-order (concat (into [] split-left) el3 (into [] split-right))
            ]
         (when (= 0 (mod step 1e4))
           (print step)
           )
         (recur (rotate-until logical-order idx (nth state idx))
                (mod (inc idx) (count state)) (inc step))
      )
      )
    )
  )

(defn -main
  "Advent of Code. Day 23"
  [& args]
  (let [inp-string "942387615"
        inp-num (into [] (map #(Integer/parseInt (str %)) inp-string))
        result-1 (apply str (map str (drop 1 (rotate-until (simulate-part1 inp-num 100) 0 1))))]
        ;result-2 (simulate-part1 (into [] (concat inp-num (into [] (range (inc (apply max inp-num)) (inc 1e6))))) 10e6)
        ;idx-1 (.indexOf result-2 1)]
    (println "Result of part 1:" result-1)
    ; TODO: part 2 (println (nth result-2 idx-1) (nth result-2 (inc idx-1)) (nth result-2 (+ 2 idx-1)))
    )
  )
