(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])

(defn initialize-computer [code]
  {
     :ip 0
     :accumulator 0
     :code code
     :visited #{}  ; set of visited instructions
     :executions 0 ; count how many instructions were executed so far
     :limit 1000
     :exit 0
  })


(defn nop [computer]
  (assoc computer :ip (+ (get computer :ip) 1))
  )

(defn jmp [computer index]
  (assoc computer :ip (+ (get computer :ip) index))
  )

(defn acc [computer value]
  (assoc (assoc computer :ip (+ (get computer :ip) 1))
         :accumulator (+ value (get computer :accumulator)))
  )

(defn execute-part1
  "Executes instructions until one instruction attempts to be executed twice"
  [computer]
    (if (contains? (get computer :visited) (get computer :ip))
      computer
    (let [mod-computer (assoc (assoc computer :visited (conj (get computer :visited) (get computer :ip)))
                                 :executions (+ (get computer :executions) 1))]
    ; get instruction at index - execute it an return new mod-computer state then recur
      (let [instruction (nth (get mod-computer :code) (get mod-computer :ip))]
        (case (get instruction :code)
          "nop" (execute-part1 (nop mod-computer))
          "jmp" (execute-part1 (jmp mod-computer (get instruction :arg1)))
          "acc" (execute-part1 (acc mod-computer (get instruction :arg1)))
          )
        )
      )
    )
  )

(defn execute-part2
  "Executes instructions until ip is one position beyon code length"
  [computer]
    (if (= (get computer :ip) (count (get computer :code)))
      (assoc computer :exit 0)
      (if (> (get computer :executions) (get computer :limit))
        (assoc computer :exit 1)
        (let [mod-computer (assoc (assoc computer :visited (conj (get computer :visited) (get computer :ip)))
                                   :executions (+ (get computer :executions) 1))]
          (let [instruction (nth (get mod-computer :code) (get mod-computer :ip))]
            (case (get instruction :code)
              "nop" (execute-part2 (nop mod-computer))
              "jmp" (execute-part2 (jmp mod-computer (get instruction :arg1)))
              "acc" (execute-part2 (acc mod-computer (get instruction :arg1)))
              )
            )
          )
        )
      )
    )

(defn parse-instruction 
  "Parse instruction of form 'arg +/- num'"
  [line]
  (let [[cmd arg] (str/split line #" " 2)]
    {:code cmd :arg1 (Integer/parseInt arg)}
    )
  )

(defn load-code [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map parse-instruction (line-seq rdr)))
    )
  )

(defn mutate-code 
  "if nth instruction is nop convert it to jmp and vice-versa"
  [ip code]
  (let [instruction (nth code ip)]
    (if (= "jmp" (get instruction :code))
      (assoc code ip (assoc instruction :code "nop"))
      (if (= "nop" (get instruction :code))
        (assoc code ip (assoc instruction :code "jmp"))
        nil
        )
      )
    )
  )

(defn -main
  "Advent of Code. Exercise 8"
  [& args]
  (let [code (load-code "input.txt")
        code1 (load-code "input.txt")
        computer (initialize-computer code)
        all-mutations (filter #(some? %) (map #(mutate-code % code1) (range (count code1))))
        computer1 (initialize-computer code1)
        result (execute-part1 computer)
        all-executions (filter #(= 0 (get % :exit)) (map #(execute-part2 (initialize-computer %)) all-mutations))
        ]
    (println "Result of part 1:" (get result :accumulator))
    (println "Result of part 2:" (get (first all-executions) :accumulator))
    )
  )
