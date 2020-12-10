(ns exercise.core
  (:gen-class))

(defn load-map [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (reduce conj [] (map #(Integer/parseInt %) (line-seq rdr)))
    )
 )

(defn sum-difference 
  "Part 1: Calculate consecutive list differences. Count amount of 1 and 3 differences then multiply counts together"
  [numbers]
  (let [sorted-numbers (sort numbers)
        offset-numbers (next sorted-numbers)
		differences (map #(- (nth offset-numbers %) (nth sorted-numbers %)) (range (count offset-numbers)))
        all-differences (conj differences 3 (first sorted-numbers))]
    (* (count (filter #(= 1 %) all-differences)) (count (filter #(= 3 %) all-differences)))
  )
)

(defn drop-until-match 
  "Shorthand for a list [1 2 3] drop all elements until a match is found"
  [numbers match]
  (if (= (first numbers) match)
    numbers
    (drop-until-match (rest numbers) match)
  )
)

(defn count-variations-reverse 
  "Part 2: Find variations of numbers with an additional constraint that two consecutive numbers must be at most 3 apart
   Solution is partially inspired by dynamic programming. Reverse the list, then build result bottom up.

   For each number check all reachable consecutive numbers and store result in hashmap. Instead of recomputing
   results use computed solutions when moving towards first number.
  "
  [numbers partial-results idx]
  (if (>= idx (count numbers))
    (let []
      (get partial-results (nth numbers (- idx 1)))
    )
    (let [n1 (nth numbers idx)
          reachable (filter #(and (> % n1) (<= % (+ n1 3))) numbers)
          reachable-count (max 1 (reduce + (map #(get partial-results % 0) reachable)))
          new-results (assoc partial-results n1 reachable-count)
          ]
        (let []
           (count-variations-reverse numbers new-results (+ idx 1))
         )
    )
  )
)

(defn -main
  "Advent of Code 2020. Exercise 10."
  [& args]
  (let [sample-data (load-map "input.txt")
        reverse-data (reverse (conj (sort sample-data) 0))]
    (println "Solution of part 1:" (sum-difference sample-data))
    (println "Solution of part 2:" (count-variations-reverse reverse-data {} 0))
  )
)
