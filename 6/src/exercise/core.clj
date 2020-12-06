(ns exercise.core
  (:gen-class))

(defn load-answers [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [lines (flatten (reduce conj [] (line-seq rdr)))
          partitioned-list (partition-by #(empty? %) lines)
          non-empty-list (filter #(not (empty? (first %))) partitioned-list)
          ]
		  non-empty-list
        )
      )
  )

(defn count-answers [group]
  (count (into #{} (reduce concat "" group)))
  )

(defn count-true-answers [all-possible-answers group]
  (if (empty? all-possible-answers)
    0
    (let [current-answer (first all-possible-answers)
		  filtered-group-answers (filter #(.contains % (apply str (seq [current-answer]))) group)]
      (+ (if (= (count group) (count filtered-group-answers)) 1 0) 
		 (count-true-answers (rest all-possible-answers) group))
    )
  )
)

(defn count-all-true-answers [group]
  (let [all-possible-answers (into [] (into #{} (reduce concat "" group)))
		group-size (count group)]
    (count-true-answers all-possible-answers group)
  )
)

(defn -main
  "Advent of Code 2020: Exercise 6"
  [& args]
  (let [group-answers (load-answers "input.txt")]
    (println "Solution 1:" (reduce + (map count-answers group-answers)))
    (println "Solution 2:" (reduce + (map count-all-true-answers group-answers)))
  )
)
