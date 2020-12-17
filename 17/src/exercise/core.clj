(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

(defn unpack-row-dimension 
  "Convert row into 3 dimensional map from (x,y,z) (row column zet) -> # or .
   I decided to make this index because this will make it easier to get neighbours for Game of Life rules"
  [row-index row]
 (reduce merge {} (map-indexed (fn [col-index col] {(list row-index col-index 0) col}) row))
 )

(defn print-dimension 
  "prints out the same structure as is displayed in file"
  [dimension-map z]
  (println "z:" z)
  (println (keys dimension-map))
  (println (map #(nth % 0) (filter #(= (nth % 2) z) (keys dimension-map))))
  (println (map #(nth % 1) (filter #(= (nth % 2) z) (keys dimension-map))))
  (let [filtered-map-keys (filter #(= (nth % 2) z) (keys dimension-map))
        max-row (apply max (map #(nth % 0) filtered-map-keys))
        max-col (apply max (map #(nth % 1) filtered-map-keys))]
    (loop [row 0]
      (when (<= row max-row)
        (println (str/join "" (map #(get dimension-map [row % z] \.) (range (inc max-col)))))
        (recur (inc row))
      )
    )
  )
)

(defn load-pocket-dimension [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce merge {} (map-indexed unpack-row-dimension (line-seq rdr)))
  )
)
     
(defn sum-coords [c1 c2]
  (into [] (map #(+ (nth c1 %) (nth c2 %)) (range (count c1))))
  )


(defn expand-map [dimension-map init-turn]
  ;; this is a failure - max-range should be row length (since problem statement is a square)
  ;; also instead of storing all values only store active nodes
  (let [max-range 8
        turn (+ init-turn 10)]
  (loop [expanded-map dimension-map
         expanded-coords (combo/cartesian-product (range 0 (+ turn max-range)) (range 0 (+ turn max-range)) (range (- 0 (* 2 turn)) (* 2 turn)))]
    (if (empty? expanded-coords)
      expanded-map
      (let [coord (into [] (first expanded-coords))
            value (get dimension-map (sum-coords coord [-1 -1 0]) \. ) ]
        (recur (assoc expanded-map coord value) (rest expanded-coords))
        )
      )
    )
  )
)

(defn calculate-new-state [old-state active-neighbours] 
  (if (= \# old-state)
    (if (and (>= active-neighbours 2) (<= active-neighbours 3))
      \#
      \.
       )
    (if (= active-neighbours 3)
      \#
      \.
    )
  )
)

(defn play-round [dimension-map]
  (let [filtered-map-keys (keys dimension-map)
        neighbours (filter #(not= [0 0 0] (into [] %) ) (combo/cartesian-product (range -1 2) (range -1 2) (range -1 2)))]
      (loop [map-keys filtered-map-keys
             new-map dimension-map]
        (if (empty? map-keys)
          new-map
          (let [current-key (first map-keys)
                current-neighbour-coords (map #(sum-coords current-key %) neighbours)
                current-neighbours (map #(get dimension-map %) current-neighbour-coords)
                active-neighbours (count (filter #(= \# %) current-neighbours))]
            (recur (rest map-keys) (assoc new-map current-key (calculate-new-state (get dimension-map current-key) active-neighbours)))
            )
          )
        )
     )
  )

(defn play-a-game-of-life 
  "Play a game of life with max turns
   Each turn after first game increases dimension"
  [initial-dimension-map max-turns]
  (let []
    (loop [turn 1
           dimension-map initial-dimension-map]
      (if (> turn max-turns)
        dimension-map
        (let [expanded-map (expand-map dimension-map turn)]
          (recur (inc turn) (play-round expanded-map))
        )
      )
    )
  )
)

(defn -main
  "Advent of Code. Day 17"
  [& args]
  (let [pocket-dimension (load-pocket-dimension "input.txt")
        part1-state (play-a-game-of-life pocket-dimension 6)]
    (print-dimension part1-state 0)
    (println (count (filter #(= \# %) (vals part1-state))))
  )
)
