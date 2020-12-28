(ns exercise.core
  (:gen-class))

(defn parse-hand [lines]
  {:name (apply str (drop-last (first lines)))
   :hand (into [] (map #(Integer/parseInt %) (rest lines)))}
  )

(defn load-hands
  "Load hands from file
  First hand starts with <Name>:
  Followed by numbers
  Separated by newline
  Then this is repeated for 2nd player
  "
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [lines (reduce conj [] (line-seq rdr))
          first-hand (take-while #(not= "" %) lines)
          second-hand (drop 1 (drop-while #(not= "" %) lines))]
      [(parse-hand first-hand) (parse-hand second-hand)]
      )
    )
 )

(defn play-war [left-hand right-hand]
   (if (empty? left-hand)
     right-hand
     (if (empty? right-hand)
       left-hand
       (let [[left-card right-card] (list (first left-hand) (first right-hand))]
         (if (> left-card right-card)
           (play-war (into [] (concat (into [] (rest left-hand)) (list left-card right-card))) (into [] (rest right-hand)))
           (if (> right-card left-card)
             (play-war (into [] (rest left-hand)) (into [] (concat (into [] (rest right-hand)) (list right-card left-card))))
             (play-war (conj (into [] (rest left-hand) left-card) (conj (into [] (rest right-hand)) right-card)))
             )
           )
         ) 
       )
     )
   )

(defn play-war-recursive [left-hand right-hand]
   (if (empty? left-hand)
     right-hand
     (if (empty? right-hand)
       left-hand
       (let [[left-card right-card] (list (first left-hand) (first right-hand))]
         

         (if (> left-card right-card)
           (play-war-recursive (into [] (concat (into [] (rest left-hand)) (list left-card right-card))) (into [] (rest right-hand)))
           (if (> right-card left-card)
             (play-war-recursive (into [] (rest left-hand)) (into [] (concat (into [] (rest right-hand)) (list right-card left-card))))
             (play-war-recursive (conj (into [] (rest left-hand) left-card) (conj (into [] (rest right-hand)) right-card)))
             )
           )
         ) 
       )
     )
   )

(defn -main
  "Advent of Code. Day 22"
  [& args]
  (let [filename "input.txt"
        hands (load-hands filename)
        war-result (play-war (get (first hands) :hand) (get (last hands) :hand))
        ]
    (println "Result of part 1:" (reduce + (map #(* (inc %) (nth (reverse war-result) %)) (range (count war-result)))))
  )
)
