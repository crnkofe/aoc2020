(ns exercise.core
  (:gen-class))

(defn load-map [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (reduce conj [] (line-seq rdr))
    )
 )

(defn is-tree [map row col]
  (let [row (nth map row)
        row-size (count row)]
      (= (nth row (mod col row-size)) \#)
    )
  )

(defn traverse [map row col] 
  (if (>= row (count map))
    0
    (+ (if (is-tree map row col) 1 0) (traverse map (+ 1 row) (+ col 3)))
  )
)

(defn traverse-map-by [map row col delta-row delta-col]
  (if (>= row (count map))
    0
    (+ (if (is-tree map row col) 1 0) (traverse-map-by map (+ delta-row row) (+ delta-col col) delta-row delta-col))
  )
)

(defn count-trees-part2 [map offsets]
  (if (empty? offsets)
    []
    (let [[delta-row delta-col] (first offsets)]
        (conj (count-trees-part2 map (rest offsets)) (traverse-map-by map 0 0 delta-row delta-col))
      )
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [map (load-map "input.txt")
        solution1 (traverse map 0 0)
        offsets [[1 1] [1 3] [1 5] [1 7] [2 1]]
        ]
     (println "Solution 1:" solution1)
     (println "Solution 2:" (reduce * (count-trees-part2 map offsets)))
  )
  )
