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

(defn filter-x [data processed-data previous]
  (if (empty? data)
     processed-data
    (if (= previous (first data))
      (filter-x (rest data) processed-data (first data))
      (filter-x (rest data) (conj processed-data (first data)) (first data))
    )
  )
)

(defn load-timetable-pause
  "First line is earlies departure time, second line lists busses"
  [filename]
  (let [data (with-open [rdr (clojure.java.io/reader filename)]
               (reduce conj [] (line-seq rdr)))
         filtered-timetable (filter-x (str/split (nth data 1) #",") [] nil)
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


(defn -main
  "Advent of Code. Day 13"
  [& args]
  (let [timetable (load-timetable "sample.txt")
        timetable-pause (load-timetable-pause "sample.txt")
        earliest-departure (find-earliest-departure (get timetable :start-time) (get timetable :bus-lines))
 		waiting-time (- (get earliest-departure :start-time) (get timetable :start-time))]
    (println "Solution of part 1:" (* waiting-time (first (get earliest-departure :bus-lines))))
  )
)
