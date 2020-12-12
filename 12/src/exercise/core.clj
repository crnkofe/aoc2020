(ns exercise.core
  (:gen-class))

(require '[clojure.math.numeric-tower :as math])

(defn load-commands                                                                 
  "Load a square map from file"
  [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
      (reduce conj [] (map #(list (first %) (Integer/parseInt (apply str (rest %)))) (line-seq rdr)))
    )
  )

(defn create-ship []
  {:location [0 0] :direction \E :waypoint [0 0]}
)

(defn sum-points [p1 p2]
  (map #(reduce + %) (map vector p1 p2))
)

(defn diff-points [p1 p2]
  (map #(reduce - %) (map vector p1 p2))
)

(defn reflect-x [p1]
  [(- 0 (nth p1 0)) (nth p1 1)]
)

(defn reflect-y [p1]
  [(nth p1 0) (- 0 (nth p1 1))]
)

(defn calculate-manhattan [p1 p2]
  (reduce + (map #(math/abs (reduce - %)) (map vector p1 p2)))
  )

(defn move-ship [ship by]
  (assoc ship :location (sum-points (get ship :location) by))
)

(defn rotate-until-first 
  "Rotate list until first of angles = angle"
  [angles angle]
  (if (= (first angles) angle)
    angles
    (rotate-until-first (concat (rest angles) [(first angles)]) angle)
  )
)

(defn rotate-angles
  "degrees is a multiple of 90 and ranges from -270 to +270"
  [angles degrees]
  (if (= degrees 0)
    angles
    (if (> degrees 0)
      (rotate-angles (concat (rest angles) [(first angles)]) (- degrees 90))
      (rotate-angles (into [] (conj (drop-last angles) (last angles))) (+ degrees 90))
    )
  )
)

(defn rotate-ship [ship angle]
  (let [ccw-angles (into [] (rotate-until-first [\N \W \S \E] (get ship :direction)))]
    (assoc ship :direction  (first (rotate-angles ccw-angles angle)))
  )
)

(defn forward-ship [ship count-steps]
  (move-ship ship (case (get ship :direction)
      \N [0 count-steps]
      \S [0 (- 0 count-steps)]
      \E [count-steps 0]
      \W [(- 0 count-steps) 0]
    )
  )
)

(defn execute-command 
  "Action N means to move north by the given value.
   Action S means to move south by the given value.
   Action E means to move east by the given value.
   Action W means to move west by the given value.
   Action L means to turn left the given number of degrees.
   Action R means to turn right the given number of degrees.
   Action F means to move forward by the given value in the direction the ship is currently facing.
  "
  [ship command]
    (let [ [code number] command ]
      (case code
        \N (move-ship ship [0 number])
        \S (move-ship ship [0 (- 0 number)])
        \E (move-ship ship [number 0])
        \W (move-ship ship [(- 0 number) 0])
        \L (rotate-ship ship number)
        \R (rotate-ship ship (- 0 number))
        \F (forward-ship ship number)
        ship
      )
    )
  )

(defn find-quadrant
  "I define quadrants in typical math sense 
  1 | 0
  - - -
  2 | 3
  "
  [point]
  (if (>= (nth point 1) 0)
    (if (> (nth point 0) 0) 0 1)
    (if (> (nth point 0) 0) 3 2))
  )

(defn rotate-waypoint 
  "since all rotations are for 90 degrees I'll just simplify this to reflections across axis"
  [ship angle]
  (if (= angle 0)
    ship
    (let [waypoint (into [] (get ship :waypoint))]
      (if (> angle 0)
        (let [rotated-waypoint [(- 0 (nth waypoint 1)) (nth waypoint 0)]]
          (rotate-waypoint (assoc ship :waypoint rotated-waypoint) (- angle 90))
        )
        (let [rotated-waypoint [(nth waypoint 1) (- 0 (nth waypoint 0))]]
          (rotate-waypoint (assoc ship :waypoint rotated-waypoint) (+ angle 90))
        )
      )
    )
  ) 
)

(defn move-waypoint[ship by]
  (assoc ship :waypoint (sum-points (get ship :waypoint) by))
)

(defn move-towards-waypoint [ship number]
  (let [waypoint-vector (diff-points (get ship :waypoint) (get ship :location))]
    (assoc ship :location (sum-points (get ship :location) (map #(* % number) (get ship :waypoint))))
  )
)

(defn execute-command-part2
  "Action N means to move the waypoint north by the given value.
   Action S means to move the waypoint south by the given value.
   Action E means to move the waypoint east by the given value.
   Action W means to move the waypoint west by the given value.
   Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
   Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
   Action F means to move forward to the waypoint a number of times equal to the given value."
  [ship command]
    (let [ [code number] command ]
      (case code
        \N (move-waypoint ship [0 number])
        \S (move-waypoint ship [0 (- 0 number)])
        \E (move-waypoint ship [number 0])
        \W (move-waypoint ship [(- 0 number) 0])
        \L (rotate-waypoint ship number)
        \R (rotate-waypoint ship (- 0 number))
        \F (move-towards-waypoint ship number)
        ship
      )
    )
  )


(defn -main
  "Advent of Code. Day 12"
  [& args]
  (let [commands (load-commands "input.txt")]
      (println "Result of part 1: " (calculate-manhattan [0 0] (get (reduce execute-command (create-ship) commands) :location)))
      (println "Result of part 2: " (calculate-manhattan [0 0] (get (reduce execute-command-part2 (assoc (create-ship) :waypoint [10 1]) commands) :location)))
    )
  )
