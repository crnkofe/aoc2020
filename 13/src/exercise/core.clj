(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])

(defn load-timetable                                                                              
  "First line is earlies departure time, second line lists busses"
  [filename]
  (let [data (with-open [rdr (clojure.java.io/reader filename)]
      (reduce conj [] (line-seq rdr))
    )]
    {:start-time (Integer/parseInt (first data)) :bus-lines (map #(Integer/parseInt %) (filter #(not= % "x") (str/split (nth data 1) #",")))}
  )
)

(defn load-timetable-pause
  "First line is earlies departure time, second line lists busses"
  [filename]
  (let [data (with-open [rdr (clojure.java.io/reader filename)]
               (reduce conj [] (line-seq rdr)))
         filtered-timetable (str/split (nth data 1) #",")
         offsets (map #(hash-map (Integer/parseInt (first %)) (nth % 1)) (filter #(not= (first %) "x") (map #(list % (.indexOf filtered-timetable %)) filtered-timetable)))
]
    {
      :start-time (Integer/parseInt (first data)) 
      :bus-lines (map #(Integer/parseInt %) (filter #(not= % "x") (str/split (nth data 1) #",")))
      :offsets (reduce merge offsets)
    }
  )
)

(defn find-earliest-departure [ts bus-lines]
  (let [candidates (filter #(= 0 (nth % 1)) (map #(list % (mod ts %)) bus-lines))]
    (if (empty? candidates)
      (find-earliest-departure (inc ts) bus-lines)
      {:start-time ts :bus-lines [(first (first candidates))]}
    )
  )
)

(defn validate [ts offsets]
  (if (empty? offsets)
    true
    (let [max-bus (apply max (keys offsets))]
      (if (not= 0 (mod (+ (get offsets max-bus) ts) max-bus))
        false
        (validate ts (dissoc offsets max-bus))
        )
      )
    )
  )


(defn find-ideal-bus-line
  "Idea: find largest integer and iterate over multiples of that integer
   At each iteration check if all other bus-lines at offset divide with 0 remainder
   *** This unfortunately doesn't work for large numbers ***"
  [starting-ts tt limit]
  (let [max-bus-num (apply max (get tt :bus-lines))
        max-bus-offset (get (get tt :offsets) max-bus-num)
        remaining-offsets (dissoc (get tt :offsets) max-bus-num)]
    (loop [ts (* (/ starting-ts max-bus-num) max-bus-num)]
      (if (or (> ts limit) (validate (- ts max-bus-offset) remaining-offsets))
        ts
        (recur (+ ts max-bus-num))
      )
    )
  )
)


(defn smart-solution-part2 
  "Apply Chinese remainder to find the solution
  Since numbers specified are coprimer the problem can be reformulated as follows
  === means congruence
  X === a_i (mod n_i)

  To get a number that satisfies all equations follow this procedure:
  N = n_1 * n_2 ... * n_n
  y_i = N / n_i
  z_i = y_i^-1 

  The solution is x = sum_i=1..k (a_i * y_i * z_i)
  https://brilliant.org/wiki/chinese-remainder-theorem/

  An even simple solution is the following:
  make use of the following property
  start lowest bus id to largest

  find first timestamp at which ts mod bus_nr is 0  
  then increase periodicity (step)
  repeat adding step until finding next larger ts mod bus_nr being 0
  multiply existing step with current bus number (repeat until bus numbers are exhausted)
  "
  [tt start-ts]
  (loop [ts start-ts
         step 1 
         busses (sort (keys (get tt :offsets)))]
    (if (empty? busses)
      ts
      (let [current-bus (first busses)
            ts-offset (+ ts (get (get tt :offsets) current-bus))]
        (if (= 0 (mod ts-offset current-bus))
          (recur ts (* step current-bus) (rest busses)) ; increase periodicity
          (recur (+ ts step) step busses)               ; increase timestamp
        )
      )
    )
  )
)

(defn -main
  "Advent of Code. Day 13"
  [& args]
  (let [timetable (load-timetable "input.txt")
        timetable-pause (load-timetable-pause "input.txt")
        earliest-departure (find-earliest-departure (get timetable :start-time) (get timetable :bus-lines))
 		waiting-time (- (get earliest-departure :start-time) (get timetable :start-time))]
    (println "Solution of part 1:" (* waiting-time (first (get earliest-departure :bus-lines))))
    (println "Solution of part 2:" (smart-solution-part2 timetable-pause 1))
  )
)
