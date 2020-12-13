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

(defn find-earliest-departure [ts bus-lines]
  (let [candidates (filter #(= 0 (nth % 1)) (map #(list % (mod ts %)) bus-lines))]
    (if (empty? candidates)
      (find-earliest-departure (inc ts) bus-lines)
      {:start-time ts :bus-lines [(first (first candidates))]}
    )
  )
)

(defn -main
  "Advent of Code. Day 13"
  [& args]
  (let [timetable (load-timetable "input.txt")
        earliest-departure (find-earliest-departure (get timetable :start-time) (get timetable :bus-lines))
 		waiting-time (- (get earliest-departure :start-time) (get timetable :start-time))]
  (println "Solution of part 1:" (* waiting-time (first (get earliest-departure :bus-lines))))
  )
)
