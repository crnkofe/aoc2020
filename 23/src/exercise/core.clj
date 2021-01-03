(ns exercise.core
  (:gen-class))

(defn find-label
  "Crab selects a label that is at "
  [max-value val el3]
    (loop [label val]
      (if (and (>= label 1) (not (> (count (filter #(= % label) el3)) 0)))
        label
        (if (<= label 0)
          (recur max-value)
          (recur (dec label))
          )
      )
    )
  )


(defn map-to-list [hmap]
  (loop [result [1]
          n 1]
    (let [next (get (get hmap n) :next)]
      (if (= 1 next)
        result
        (recur (conj result next) next) 
      )
    )
  )
)
    

(defn simulate-part1-map
  "Try using a fake linked-list - just a hashmap where each element also points to next and previous el

  On operation just remap elements rather than search through a linked list
  "
  [in steps]
  (let [lst-map (apply hash-map (flatten (map #(list (nth in %) { 
                 :prev (nth in (mod (dec %) (count in))) 
                 :next (nth in (mod (inc %) (count in)))
                 }) (range (count in)))))
        max-value (apply max in)]
    (loop [state lst-map 
           current-val (first in)
           step 0]
      (if (>= step steps)
        (map-to-list state)
        (let [el3-1 (get (get state current-val) :next)
              el3-2 (get (get state el3-1) :next)
              el3-3 (get (get state el3-2) :next)
              el3 (list el3-1 el3-2 el3-3)
              next-label (find-label max-value (dec current-val) el3)]
              ;; remap first second and 3rd and 4th element (as if this map was a linked list)
              ;; then continue with 4th element
              (recur (reduce (fn [hmap [update-key value]] (assoc-in hmap update-key value)) state
                 [[[current-val :next] (get (get state el3-3) :next)]
                 [[(get (get state el3-3) :next) :prev] current-val]
                 [[next-label :next] el3-1]
                 [[el3-1 :prev] next-label]
                 [[el3-3 :next] (get (get state next-label) :next)]
                 [[(get state next-label) :previous] el3-3]]) (get (get state el3-3) :next) (inc step))
          )
        )
      )
    )
  )

(defn -main
  "Advent of Code. Day 23"
  [& args]
  (let [inp-string "942387615"
        ; inp-string "389125467" ;; sample input
        inp-num (into [] (seq (map #(Integer/parseInt (str %)) inp-string)))
        sim-fast-1 (simulate-part1-map inp-num 100) ]
    (println "Result of part 1:" (apply str (drop 1 sim-fast-1)))
    (let [longtail (into [] (range (inc (apply max inp-num)) (inc 1000000)))
          totalite (into inp-num longtail)]
        (let [result-2 (simulate-part1-map totalite 10000000)]
          (println "Result of part 2:" (* (nth result-2 1) (nth result-2 2)))
          )
      )
    )
  )
