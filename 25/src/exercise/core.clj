(ns exercise.core
  (:gen-class))

(defn calculate-loop-steps [modulo subject-number steps]
  (loop [value 1
         step 0]
      (if (>= step steps)
        value
        (let [new-value (mod (* value subject-number) modulo)]
          (recur new-value (inc step))
        )
    )
  )
)

(defn calculate-loop-inner [modulo subject-number target]
  (loop [value 1
         step 0]
      (if (= target value)
        step
        (let [new-value (mod (* value subject-number) modulo)]
          (recur new-value (inc step))
        )
    )
  )
)

(defn -main
  "Advent of Code. Day 25"
  [& args]
  (let [card-key 15335876 ; 5764801 ; (sample)
        door-key 15086442 ; 17807724 ; (sample)
        modulo 20201227 
        subject-number 7
        card-loop (calculate-loop-inner modulo subject-number card-key)
        door-loop (calculate-loop-inner modulo subject-number door-key)]
    (println "Result of part 1:" (calculate-loop-steps modulo door-key card-loop))
    ;; there's not part II :/
  )
)
