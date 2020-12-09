(ns exercise.core
  (:gen-class))

(require '[clojure.set :as set])

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))


(defn load-numbers [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map #(Long/parseLong %) (line-seq rdr)))
    )
  )

(defn find-all-sums  
  "Return a hash set of sums of all combinations of elements"
  [elements]
  (if (= (count elements) 1)
    #{}
	(let [remaining-elements (into [] (rest elements))
          sums (into #{} (map #(+ (first elements) %) (rest elements)))]
    	(set/union sums (find-all-sums remaining-elements))
    )
  )
)

(defn find-trespasser 
  "Find the first number that is not a sum of two previously encountered numbers"
  [processed-numbers numbers preamble-length]
  (if (empty? numbers) ; if all results are valid return nil
    nil
	(let [current-number (first numbers)
		  drop-elements (if (> (+ 1 (count processed-numbers)) preamble-length) (- (+ 1 (count processed-numbers)) preamble-length) 0)
          new-processed-numbers (into [] (drop drop-elements (conj processed-numbers current-number)))
          current-sums (find-all-sums processed-numbers)]
      (if (and (>= (count processed-numbers) preamble-length) (not (contains? current-sums current-number)))
        current-number
        (find-trespasser new-processed-numbers (rest numbers) preamble-length)
        )
      )
    )
  )

(defn find-windowed-sum
  "map every element past nth "
  [processed-numbers numbers preamble-length target-number]
  (if (empty? numbers) ; if all results are valid return nil
    nil
	(let [current-number (first numbers)
		  drop-elements (if (> (+ 1 (count processed-numbers)) preamble-length) (- (+ 1 (count processed-numbers)) preamble-length) 0)
          new-processed-numbers (into [] (drop drop-elements (conj processed-numbers current-number)))
		  ]
		(if (= target-number (reduce + processed-numbers))
          current-number
          (find-windowed-sum new-processed-numbers (rest numbers) preamble-length target-number)
        )
      )
    )
  )

(defn -main
  "Advent of Code 2020. Exercise 9"
  [& args]
  (let [numbers (load-numbers "input.txt")
        trespasser (find-trespasser [(first numbers)] (rest numbers) 25)
        trespasser-sequences (map #(list (find-windowed-sum [(first numbers)] (rest numbers) % trespasser) %) (range 2 20))
        sequence-pair (first (filter #(not (nil? (first %))) trespasser-sequences))
        numbers-from (- (.indexOf numbers (first sequence-pair)) (nth sequence-pair 1))
        tress-numbers (into [] (take (nth sequence-pair 1) (drop numbers-from numbers)))
]
	(println "Result of part 1:" trespasser)
    (println "Result of part 2:" (+ (apply min tress-numbers))
  )
)
