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

(defn -main
  "Advent of Code. Day 11"
  [& args]
  (let [input-map (load-map "input.txt")]
    ; (println (convert-map-to-str (apply-rules input-map)))
    ; (println (find-rule-change input-map))
    (println "Solution to part 1:" (find-rule-change input-map))
    )
  )
