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

(defn play-war-recursive [in-left in-right in-left-history in-right-history]
  (loop [left in-left
         right in-right
         left-history in-left-history
         right-history in-right-history]
    (let [left-hand (get left :hand)
          right-hand (get right :hand)]
       (if (or (empty? right-hand) (and (contains? left-history left) (contains? right-history right)))
         left
         (if (empty? left-hand)
           right
           (let [[left-card right-card] (list (first left-hand) (first right-hand))]
             (if (and (>= (count (rest left-hand)) left-card) (>= (count (rest right-hand)) right-card))
               ; play recursive combat if there's enough cards left
               (let [new-left {:name (get left :name) :hand (take (first left-hand) (rest left-hand))}
                     new-right {:name (get right :name) :hand (take (first right-hand) (rest right-hand))}]
                 (let [recursive-war (play-war-recursive new-left new-right #{} #{})]
                   (if (= (get left :name) (get recursive-war :name))
                     (let [next-left {:name (get left :name) :hand (into [] (concat (into [] (rest left-hand)) (list left-card right-card)))}
                      next-right {:name (get right :name) :hand (into [] (rest right-hand))}]
                       (recur next-left next-right (conj left-history left) (conj right-history right))
                     )
                     (let [next-left {:name (get left :name) :hand (into [] (rest left-hand))}
                           next-right {:name (get right :name) :hand (into [] (concat (into [] (rest right-hand)) (list right-card left-card)))}]
                       (recur next-left next-right (conj left-history left) (conj right-history right))
                     )
                   )
                 )
               )
               (if (> left-card right-card)
                 (let [next-left {:name (get left :name) :hand (into [] (concat (into [] (rest left-hand)) (list left-card right-card)))}
                       next-right {:name (get right :name) :hand (into [] (rest right-hand))}]
                   (recur next-left next-right (conj left-history left) (conj right-history right))
                 )
                 (let [next-left {:name (get left :name) :hand (into [] (rest left-hand))}
                       next-right {:name (get right :name) :hand (into [] (concat (into [] (rest right-hand)) (list right-card left-card)))}]
                   (recur next-left next-right (conj left-history left) (conj right-history right))
                 )
                 )
             )
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
        recurse-war-result (play-war-recursive (first hands) (last hands) #{} #{})
        ]
    (println "Result of part 1:" (reduce + (map #(* (inc %) (nth (reverse war-result) %)) (range (count war-result)))))
    (println "Result of part 2:" (reduce + (map #(* (inc %) (nth (reverse (get recurse-war-result :hand)) %)) (range (count (get recurse-war-result :hand))))))
  )
)
