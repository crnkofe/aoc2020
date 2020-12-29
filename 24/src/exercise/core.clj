(ns exercise.core
  (:gen-class))

(defn flip [hex-grid point]
  (if (= :white (get hex-grid point :white))
    (assoc hex-grid point :black)
    (assoc hex-grid point :white)
  )
)

(defn sum-point [p1 p2]
  [(+ (nth p1 0) (nth p2 0)) (+ (nth p1 1) (nth p2 1))]
  )


    (comment
    (case (apply str (take 2 line)) 
      "ne" (flipper-point (flip hex-grid (sum-point point [1 0])) (drop 2 line) (sum-point point [1 0]))
      "se" (flipper-point (flip hex-grid (sum-point point [-1 1])) (drop 2 line) (sum-point point [-1 1]))
      "nw" (flipper-point (flip hex-grid (sum-point point [1 -1])) (drop 2 line) (sum-point point [1 -1]))
      "sw" (flipper-point (flip hex-grid (sum-point point [-1 0])) (drop 2 line) (sum-point point [-1 0]))
      (case (apply str (take 1 line))
         "e" (flipper-point (flip hex-grid (sum-point point [0 1])) (drop 1 line) (sum-point point [0 1]))
         "w" (flipper-point (flip hex-grid (sum-point point [0 -1])) (drop 1 line) (sum-point point [0 -1]))
         hex-grid
      )
    ))

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

(defn -main
  "Advent of Code. Day 24."
  [& args]
  (let [filename "input.txt"
        loaded-map (load-map filename)]
    (print "Solution of part 1:" (count (filter #(= :black %) (vals loaded-map))))
  )
)
