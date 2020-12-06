(ns exercise.core
  (:gen-class))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn load-numbers [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map read-string (line-seq rdr)))
  )
)

(defn generate-pairs [numbers total]
  (if (< (count numbers) 2)
    []
    (let [head (first numbers)
          tail (rest numbers)
          pairs (map #(list head %) tail)]
         (filter #(= total (reduce + %)) pairs)
      )
    )
  )

(defn generate-rest-pairs [result numbers]
    (if (<= (count numbers) 2)
      (filter #(not (empty? %)) (conj result numbers))
      (let [head (first numbers)
            tail (rest numbers)
            pairs (map #(conj [head] %) tail)]
           (generate-rest-pairs (concat pairs result) tail)
        )
      )
    )

(defn generate-triples [numbers]
  (if (<= (count numbers) 3)
     [numbers]
  (let [head (first numbers)
        tail (rest numbers)
        pairs (generate-rest-pairs [] tail)        
        triples (map #(concat [head] %) pairs)
        result (concat (generate-triples tail) triples)]
      result
    )
  )
)


(defn find-combo [result numbers total]
  (if (< (count numbers) 2)
    result
    (let [head (first numbers)
          tail (rest numbers)
          pairs (generate-pairs numbers total)
          first-pair (first pairs)]
         (if (some? first-pair)
           first-pair
           (find-combo [] tail total)
           )
      )
    )
  )

(defn find-triple-combo [numbers total]
      (let [triples (generate-triples numbers)]
          (first (filter #(= total (reduce + %)) triples))
      )
    )
  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [numbers (load-numbers "/home/simonm/github/aoc2020/1/input")
        pairs (generate-pairs numbers 2020)]
    (let [result (find-combo [] numbers 2020)]
      (print "Result of part 1: ")
      (println (reduce * result))
      (print "Result of part 2: ")
      (println (reduce * (find-triple-combo numbers 2020)))
      )
    )
  )
