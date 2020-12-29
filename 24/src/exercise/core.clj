(ns exercise.core
  (:gen-class))

(require '[clojure.math.combinatorics :as combo])

(defn flip [hex-grid point]
  (if (= :white (get hex-grid point :white))
    (assoc hex-grid point :black)
    (assoc hex-grid point :white)
  )
)

(defn sum-point [p1 p2]
  [(+ (nth p1 0) (nth p2 0)) (+ (nth p1 1) (nth p2 1))]
  )

(defn flipper-point [hex-grid line point]
  (let [direction (take 2 line)
        col2 (mod (nth point 1) 2)]
    (case (apply str (take 2 line)) 
      "ne" (flipper-point hex-grid (drop 2 line) (sum-point point [1 0]))
      "se" (flipper-point hex-grid (drop 2 line) (sum-point point [-1 1]))
      "nw" (flipper-point hex-grid (drop 2 line) (sum-point point [1 -1]))
      "sw" (flipper-point hex-grid (drop 2 line) (sum-point point [-1 0]))
      (case (apply str (take 1 line))
         "e" (flipper-point hex-grid (drop 1 line) (sum-point point [0 1]))
         "w" (flipper-point hex-grid (drop 1 line) (sum-point point [0 -1]))
         (flip hex-grid point)
      )
    )
  )
)

(defn flipper [hex-grid line]
  (flipper-point hex-grid line [0 0])
)

(defn load-map
  "Load a hexagonal grid"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (loop [hex-grid {}
           lines (line-seq rdr)]
      (if (empty? lines)
        hex-grid
        (let [first-line (first lines)]
          (recur (flipper hex-grid first-line) (rest lines))
        )
      )
    )
  )
)

(defn flip-on-rules [point hex-grid]
  (let [color (get hex-grid point :white)
        neighbours (filter #(not= nil %) (map #(get hex-grid (sum-point point %) :nil) [[1 0] [-1 1] [1 -1] [-1 0] [0 1] [0 -1]]))
        count-black (count (filter #(= :black %) neighbours))]
        ; count-white (count (filter #(= :white %) neighbours))]
      (if (and (= color :white) (= count-black 2))
        [point :black]
        (if (and (= color :black) (or (= 0 count-black) (> count-black 2)))
          [point :white]
          [point color]
          )
        )
      )
    )

(defn play-round [hex-grid]
  ;; TODO: figure out how to fight stack overflow
  (let [extended-grid-keys (map #(apply sum-point %) (into #{} (combo/cartesian-product (keys hex-grid) [[1 0] [-1 1] [1 -1] [-1 0] [0 1] [0 -1]])))
        flipped-map (map #(flip-on-rules [(first %) (second %)] hex-grid) extended-grid-keys)]
    ;(println extended-grid-keys)
    (apply hash-map (reduce concat [] flipped-map))
  )
)

(defn play-rounds [in-hex-grid rounds]
  (loop [hex-grid in-hex-grid
         round 0]
    (if (> round rounds)
      hex-grid
      (recur (play-round hex-grid) (inc round))
    )
  )
)

(defn -main
  "Advent of Code. Day 24."
  [& args]
  (let [filename "sample.txt"
        loaded-map (load-map filename)
        interactive-map (play-rounds loaded-map 5)]
    (print "Solution of part 1:" (count (filter #(= :black %) (vals loaded-map))))
    (println interactive-map)
    (print "Solution of part 2:" (count (filter #(= :black %) (vals interactive-map))))
  )
)
