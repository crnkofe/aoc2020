(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

(defn load-map
  "Load a square map from file
   Legend:
   . floor
   # occupied seat
   L unoccupied seat"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map #(into [] %) (line-seq rdr)))
  )
)

(defn convert-map-to-str [plane-map]
  "Convert loaded map back to string form
   Legend:
   . floor
   # occupied seat
   L unoccupied seat"
  (str/join "\n" (into [] (map #(str/join "" %) plane-map)))
  )

(defn adjacent-coords [row-index col-index row-count col-count]
  (let [neighbours [[(+ row-index 1) col-index] [(- row-index 1) col-index]
                    [row-index (+ col-index 1)] [row-index (- col-index 1)] 
                    [(+ row-index 1) (+ col-index 1)] [(- row-index 1) (- col-index 1)]
                    [(+ row-index 1) (- col-index 1)] [(- row-index 1) (+ col-index 1)]]]
    (filter #(and (and (>= (nth % 0) 0) (>= (nth % 1) 0))
                  (and (< (nth % 0) row-count) (< (nth % 1) col-count))) neighbours)
  )
)


(defn apply-single-rule
   "If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.
   " 
   [full-map row-index col-index]
  (let [row (nth full-map row-index)
        c (nth row col-index)
        adjacent (adjacent-coords row-index col-index (count full-map) (count row))
        neighbours (filter #(or (= % \L) (= % \#)) (map #(nth (nth full-map (nth % 0)) (nth % 1)) adjacent))
        occupied (count (filter #(= \# %) neighbours))] 
    (if (and (= \L c) (= occupied 0))
      \#
      (if (and (= \# c) (>= occupied 4))
        \L
        c
        )
      )
    )
  )

(defn apply-row [full-map row-index]
  (into [] (map #(apply-single-rule full-map row-index %)) (range (count (first full-map))))
  )

(defn apply-rules [full-map]
  (let [map-size (count full-map)]
    (map #(apply-row full-map %) (range map-size))
  )
  )

(defn find-rule-change [full-map]
  (let [mutated-map (apply-rules full-map)]
    (if (= (flatten full-map) (flatten mutated-map))
      (count (filter #(= \# %) (flatten  (reduce conj [] mutated-map))))
      (find-rule-change mutated-map)
      )
    )
  )


(defn is-valid-coord [point row-count col-count]
  (and (and (>= (nth point 0) 0) (>= (nth point 1) 0))
                  (and (< (nth point 0) row-count) (< (nth point 1) col-count)))
  )

(defn find-seat [full-map point direction row-count col-count] 
  (if (not (is-valid-coord point row-count col-count))
    []
    (let [c (nth (nth full-map (nth point 0)) (nth point 1))]
    (if (or (= c \L) (= c \#))
      point
      (let [new-point [(+ (nth point 0) (nth direction 0)) (+ (nth point 1) (nth direction 1))]]
        (find-seat full-map new-point direction row-count col-count)
        )
      )
    )
  )
)

(defn queen-occupied-coords [full-map row-index col-index row-count col-count]
  (let [neighbours [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]]
        queen-results (map #(find-seat full-map [(+ row-index (nth % 0)) (+ col-index (nth % 1))] % row-count col-count) neighbours)
        ]
    (filter #(not (empty? %)) queen-results)
  )
)

(defn apply-queen-rule
   "If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    If a seat is occupied (#) and four or more seats visible in 8 directions around to it are also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.
   " 
   [full-map row-index col-index]
  (let [row (nth full-map row-index)
        c (nth row col-index)
        adjacent (queen-occupied-coords full-map row-index col-index (count full-map) (count row)) 
        neighbours (filter #(or (= % \L) (= % \#)) (map #(nth (nth full-map (nth % 0)) (nth % 1)) adjacent))
        occupied (count (filter #(= \# %) neighbours))] 
    (if (and (= \L c) (= occupied 0))
      \#
      (if (and (= \# c) (>= occupied 5))
        \L
        c
        )
      )
    )
  )


(defn apply-queen-row [full-map row-index]
  (into [] (map #(apply-queen-rule full-map row-index %)) (range (count (first full-map))))
  )

(defn apply-queen-rules [full-map]
  (let [map-size (count full-map)]
    (map #(apply-queen-row full-map %) (range map-size))
  )
  )

(defn find-queen-rule-change [full-map limit]
  (let [mutated-map (apply-queen-rules full-map)]
    (if (or (<= limit 1) (= (flatten full-map) (flatten mutated-map)))
      (count (filter #(= \# %) (flatten  (reduce conj [] mutated-map))))
      (find-queen-rule-change mutated-map (- limit 1))
      )
    )
  )



(defn -main
  "Advent of Code. Day 11"
  [& args]
  (let [input-map (load-map "input.txt")]
    (println "Relax as this may take a while...")
    (println "Solution to part 1:" (find-rule-change input-map))
    (println "Solution to part 2:" (find-queen-rule-change input-map 1000))
    )
  )
