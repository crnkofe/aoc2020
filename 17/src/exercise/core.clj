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

(defn generate-keys 
  "find min, max for each dimension and generate all keys between +-min-1 max+1"
   [dimension-keys]
  
   (let [min-x (apply min (map #(nth % 0) dimension-keys))
         max-x (apply max (map #(nth % 0) dimension-keys))
         min-y (apply min (map #(nth % 1) dimension-keys))
         max-y (apply max (map #(nth % 1) dimension-keys))
         min-z (apply min (map #(nth % 2) dimension-keys))
         max-z (apply max (map #(nth % 2) dimension-keys))
         range-x [(- min-x 2) (+ 2 max-x)]
         range-y [(- min-y 2) (+ 2 max-y)]
         range-z [(- min-z 2) (+ 2 max-z)]
         ]
   (map #(into [] %) (combo/cartesian-product 
                       (range (nth range-x 0) (nth range-x 1))
                       (range (nth range-y 0) (nth range-y 1)) 
                       (range (nth range-z 0) (nth range-z 1))))
  )
)

(defn play-round [dimension-map]
  (let [filtered-map-keys (generate-keys (keys dimension-map))
        neighbours (filter #(not= [0 0 0] (into [] %) ) (combo/cartesian-product (range -1 2) (range -1 2) (range -1 2)))]
      (loop [map-keys filtered-map-keys
             new-map dimension-map]
        (if (empty? map-keys)
          new-map
          (let [current-key (first map-keys)
                current-neighbour-coords (map #(sum-coords current-key %) neighbours)
                current-neighbours (map #(get dimension-map %) current-neighbour-coords)
                active-neighbours (count (filter #(= \# %) current-neighbours))
                new-state (calculate-new-state (get dimension-map current-key) active-neighbours)]
            (recur (rest map-keys) (assoc new-map current-key new-state))
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
        (recur (inc turn) (play-round dimension-map))
      )
    )
  )
)


(defn unpack-row-dimension-4
  "Convert row into 3 dimensional map from (x,y,z) (row column zet) -> # or .
   I decided to make this index because this will make it easier to get neighbours for Game of Life rules"
  [row-index row]
 (reduce merge {} (map-indexed (fn [col-index col] {(list row-index col-index 0 0) col}) row))
 )

(defn load-pocket-dimension-4 [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce merge {} (map-indexed unpack-row-dimension-4 (line-seq rdr)))
  )
)
     

(defn generate-keys-4
  "find min, max for each dimension and generate all keys between +-min-1 max+1"
   [dimension-keys]
  
   (let [min-x (apply min (map #(nth % 0) dimension-keys))
         max-x (apply max (map #(nth % 0) dimension-keys))
         min-y (apply min (map #(nth % 1) dimension-keys))
         max-y (apply max (map #(nth % 1) dimension-keys))
         min-z (apply min (map #(nth % 2) dimension-keys))
         max-z (apply max (map #(nth % 2) dimension-keys))
         min-w (apply min (map #(nth % 3) dimension-keys))
         max-w (apply max (map #(nth % 3) dimension-keys))
         range-x [(- min-x 2) (+ 2 max-x)]
         range-y [(- min-y 2) (+ 2 max-y)]
         range-z [(- min-z 2) (+ 2 max-z)]
         range-w [(- min-w 2) (+ 2 max-w)]
         ]
   (map #(into [] %) (combo/cartesian-product 
                       (range (nth range-x 0) (nth range-x 1))
                       (range (nth range-y 0) (nth range-y 1)) 
                       (range (nth range-z 0) (nth range-z 1))
                       (range (nth range-w 0) (nth range-w 1))))
  )
)

(defn play-round-4 [dimension-map]
  (let [filtered-map-keys (generate-keys-4 (keys dimension-map))
        neighbours (filter #(not= [0 0 0 0] (into [] %) ) (combo/cartesian-product (range -1 2) (range -1 2) (range -1 2) (range -1 2)))]
      (loop [map-keys filtered-map-keys
             new-map dimension-map]
        (if (empty? map-keys)
          new-map
          (let [current-key (first map-keys)
                current-neighbour-coords (map #(sum-coords current-key %) neighbours)
                current-neighbours (map #(get dimension-map %) current-neighbour-coords)
                active-neighbours (count (filter #(= \# %) current-neighbours))
                new-state (calculate-new-state (get dimension-map current-key) active-neighbours)]
            (recur (rest map-keys) (assoc new-map current-key new-state))
            )
          )
        )
     )
  )


(defn play-a-game-of-life-4
  "Play a game of life with max turns
   Each turn after first game increases dimension"
  [initial-dimension-map max-turns]
  (let []
    (loop [turn 1
           dimension-map initial-dimension-map]
      (println "turn" turn)
      (if (> turn max-turns)
        dimension-map
        (recur (inc turn) (play-round-4 dimension-map))
      )
    )
  )
)



(defn -main
  "Advent of Code. Day 17"
  [& args]
  (let [pocket-dimension (load-pocket-dimension "input.txt")
        part1-state (play-a-game-of-life pocket-dimension 6)
        pocket-dimension-4 (load-pocket-dimension-4 "input.txt") 
        part2-state (play-a-game-of-life-4 pocket-dimension-4 6)
        ]
    (println "Result of part 1:" (count (filter #(= \# %) (vals part1-state))))
    (println "Result of part 2:" (count (filter #(= \# %) (vals part2-state))))
  )
)
