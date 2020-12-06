(ns exercise.core
  (:gen-class))

(require '[clojure.set :as set])

(defn tree-to-number [s low high]
  (if (empty? s)
    low
    (if (or (= \F (first s)) (= \L (first s)))
      (tree-to-number (rest s) low (+ low (quot (- high low) 2)))
      (tree-to-number (rest s) (+ 1 (+ low (quot (- high low) 2))) high)
      )
    )
  )

(defn parse-line [line]
  (let [row-part (subs line 0 7)
        col-part (subs line 7 (count line))]
     (+ ( * 8 (tree-to-number row-part 0 127)) (tree-to-number col-part 0 7))
    )
  )


(defn load-seats [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq rdr))
    )
  ) 

(defn -main
  "Solution to AOC 2020, Exercise 5"
  [& args]
  (let [seats (load-seats "input.txt")
        indices (map parse-line seats)
        min-index (apply min indices)
        max-index (apply max indices)]
      (println "Solution 1:" (apply max indices))
      (println "Solution 2:" (set/difference (into #{} (range min-index max-index)) (into #{} (sort indices))))
    )
  )
