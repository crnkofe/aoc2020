(ns exercise.core
  (:gen-class))

(defn split-right
  "gets element at idx mod (count in )"
  [in idx]
  ;(println in)
  (let [sz (count in)
        mod-idx (mod idx sz)
        last-count (min (- sz idx) 3)
        first-count (- 3 last-count)]
    ;(println "lc" last-count first-count mod-idx (subvec in mod-idx (+ mod-idx 3)))
    (if (>= (- sz mod-idx) 3)
      (let [result [[mod-idx (+ mod-idx 3)]
                   (subvec in mod-idx (+ mod-idx 3))]]
        ; (println mod-idx (+ mod-idx 3) result)
        result
      )
      [[(- (count in) last-count) first-count]
       (into (into [] (subvec in (- (count in) last-count) (count in))) (subvec in 0 first-count))
       ]
      )
    )
  )

(defn find-label
  "Crab selects a label that is at "
  [in val el3]
  (let [destination-label val ]
    (loop [label destination-label]
      (if (and (>= label 1) (not (> (count (filter #(= % label) el3)) 0)))
        label
        (if (<= label 0)
          (recur (apply max in))
          (recur (dec label))
          )
      )
    )
  )
)

(defn find-label-max
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


(defn rotate-until [in idx el]
  ; rotate until el is at index in list
  (let [curr-idx (.indexOf in el)]
    (if (> curr-idx idx)
      (let [diff (- curr-idx idx)]
        (concat 
          (take (- (count in) diff) (drop diff in)) 
          (take diff in))
      )
      (if (< curr-idx idx)
        (let [diff (- idx curr-idx)]
          (concat (take-last diff in) (take (- (count in) diff) in))
        )
        in
        )
      )
    )
  )

(defn simulate-part1 [in steps]
  (loop [state (into [] in)
         idx 0
         step 0]
    (when (= (mod step 1000) 0)
      (println "step" (inc step)) ;  state (.indexOf state 1))
      )
    (if (or (>= step steps))
      state
      (let [[[el3-start el3-end] el3] (split-right state (mod (inc idx) (count state)))]
        (println el3-start el3-end)
        (let [destination-label (find-label state (dec (nth state (mod idx (count state)))) el3)
              split-index (mod (inc (.indexOf state destination-label)) (count state))]
          (println "split" split-index destination-label el3-start el3-end)
          ;(println "ah" state (inc idx) el3-start el3-end el3 split-index destination-label)
          (if (> el3-start el3-end)
              (let []
                (comment
                (println "case1")
                (println "split0" (subvec state 0 split-index))
                (println "split0" (into [] (flatten el3)))
                (println "split0" (subvec state split-index el3-start))
                (println "split0" (subvec state el3-end (count state)))
                )
              (let [logical-order (into (subvec state el3-end split-index) 
                                        (concat (into [] (flatten el3))
                                              (subvec state split-index el3-start)
                                        ))
                    next-idx (mod (inc (.indexOf logical-order (nth state idx))) (count state))]
               (println "case1")
               (recur logical-order next-idx (inc step))
               )
              )
            (if (<= split-index el3-start)
              (let []
                (comment 
                (println "case2")
                (println "split1" (subvec state 0 split-index))
                (println "split1" (into [] (flatten el3)))
                (println "split1" (subvec state split-index el3-start))
                (println "split1" (subvec state el3-end (count state)))
                )
              (let [logical-order (into (subvec state 0 split-index) 
                                        (into (into [] (flatten el3))
                                        (into (subvec state split-index el3-start)
                                              (subvec state el3-end (count state)))
                                        ))
                    next-idx (mod (inc (.indexOf logical-order (nth state idx))) (count state))]
               (println "case2")
               (recur logical-order next-idx (inc step))
               )
              )
              (let []
                (comment
                (println "case3" split-index)
                (println "split2" (subvec state 0 el3-start))
                (println "split2" (subvec state el3-end split-index))
                (println "split2" (flatten el3))
                (println "split2" (subvec state split-index (count state)))
                (println state idx)
                (println (into (subvec state 0 el3-start)
                                        (into (subvec state el3-end split-index) 
                                              (into (into [] (flatten el3))
                                                    (subvec state split-index (count state)))
                                        )))
                )
                (let [logical-order (into (subvec state 0 el3-start)
                                        (into (subvec state el3-end split-index) 
                                              (into (into [] (flatten el3))
                                                    (subvec state split-index (count state)))
                                        ))
                    next-idx (mod (inc (.indexOf logical-order (nth state idx))) (count state))]
               (println "case3" )
               (recur logical-order next-idx (inc step))
               )
              )
             )
          )
        )
      )
    )
  )
)

(comment
(recur (concat (take (- (inc idx) 4) (drop 4 state))
              el3 
              (drop (inc idx) state) 
              [ (first state)] ) (inc step))
(recur (into [] (concat (subvec state 4 (inc idx)) 
                       el3 
                       (subvec state (inc idx) (count state)) 
                       [(first state)] )) (inc step))
(recur (into (into (subvec state 4 (inc idx)) 
                  (into [] el3)) 
            (conj (subvec state (inc idx) (count state)) (first state) )) (inc step))
(let [left-part (into (subvec state 4 (inc idx)) el3)]
 (let [right-part (conj (subvec state (inc idx) (count state)) (first state) )]
   (recur (into left-part right-part) (inc step))
 )
)
)

(defn simulate-part1-rotator 
  "This is a different approach than before
  Instead of using any indices just keep rotating the datastructure
  First element goes to last place every time
  Next three elements get inserted at index of (first element - 1)
  This is usually very close to start of collection

  If first element is 1 then find max of collection and insert there (this happens once every 1mio since
  state is very large so should be relatively rare

  This works but not fast enough - would finish in 1 week
  "
  [in steps]
  (loop [state (into [] in)
         step 0]
    (when (= 0 (mod step 10))
       (let [idx-1 (.indexOf state 1)]
         (when (> idx-1 0)
           (println step (nth state idx-1) 
                    (nth state (mod (inc idx-1) (count state))) 
                    (nth state (mod (+ 2 idx-1) (count state))))
         )
         )
       )
    (println "a")
    (if (>= step steps)
      state
      (let [el3 (take 3 (drop 1 state))
            target-el-cand (dec (first state))
            target-el (find-label state target-el-cand el3)]
          (let [idx (.indexOf state target-el)]
            (let [lft (into (subvec state 4 (inc idx)) el3)
                  rgt (subvec state (inc idx) (count state))
                  res (into (into lft rgt) [(first state)])]
              (println "lft" (count lft) "rght" (count rgt))
             (recur res (inc step))
             )
            )
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
    (println lst-map)
    (loop [state lst-map 
           current-val (first in)
           step 0]
      (if (>= step steps)
        state
        (let [el3-1 (get (get lst-map current-val) :next)
              el3-2 (get (get lst-map el3-1) :next)
              el3-3 (get (get lst-map el3-2) :next)
              el3 (list el3-1 el3-2 el3-3)
              next-label (find-label-max max-value (dec current-val) el3)]
            ;; remap first second and 4th element then recur with next-label
            (println "MEGA" el3 next-label)
              (recur (reduce (fn [hmap [update-key value]]  (println update-key value)(assoc-in hmap update-key value)) state
                 [[[current-val :next] (get (get lst-map el3-3) :next)]
                 [[(get el3-3 next) :prev] current-val]
                 [[next-label :next] el3-1]
                 [[el3-1 :prev] next-label]
                 [[el3-3 :next] (get (get state next-label) :next)]
                 [[(get state next-label) :previous] el3-3]]) next-label (inc step))
          )
        )
      )
    )
  )


(defn find-label-multi
  "Crab selects a label that is at "
  [in in2 val el3]
  (let [destination-label val ]
    (loop [label destination-label]
      (if (and (>= label 1) (not (> (count (filter #(= % label) el3)) 0)))
        label
        (if (<= label 0)
          (let []
 ;           (println in in2) ;(apply max in) (apply max in2))
          (recur (max (apply max in) (if (empty? in2) 0 (apply max in2))))
          )
          (recur (dec label))
          )
      )
    )
  )
)

(defn simulate-part1-multi
  "
  Instead of using any indices just keep rotating the datastructure
  First element goes to last place every time
  Next three elements get inserted at index of (first element - 1)
  This is usually very close to start of collection

  If first element is 1 then find max of collection and insert there (this happens once every 1mio since
  state is very large so should be relatively rare

  This version uses two lists (small and large - large is a sequence, small is a vector)
  "
  [in steps]
  (loop [state-small (into [] (drop-last 10000 in))
         state-big (into [] (take-last 10000 in))
         step 0]
    (when (= 0 (mod step 1000))
      (println (inc step) (count state-small) (count state-big))
       (let [idx-1 (.indexOf state-small 1)]
         (when (> idx-1 0)
           (println (nth state-small idx-1) 
                    (nth state-small (mod (inc idx-1) (count state-small))) 
                    (nth state-small (mod (+ 2 idx-1) (count state-small))))
         )
         )
       (let [idx-1 (.indexOf state-big 1)]
         (when (> idx-1 0)
           (println (nth state-big idx-1) 
                    (nth state-big (mod (inc idx-1) (count state-big))) 
                    (nth state-big (mod (+ 2 idx-1) (count state-big))))
         )
         )
      )
    ; (println "state'small" (count state-small) (first state-small))
    ; (println "mja" (inc step) state-small) ;  state-big) ; state)
    (if (>= step steps)
      (into state-small state-big)
      (if (or (< (count state-small) 4) (> (count state-big) 15000))
        (recur (into state-small (take 9000 state-big)) (into [] (drop 9000 state-big)) step)
        (let [el3 (take 3 (drop 1 state-small))
              target-el-cand (dec (first state-small))
              target-el (find-label-multi state-small state-big target-el-cand el3)]
            (let [idx (.indexOf state-small target-el)]
              ; (println target-el idx (take 10 state-small))
              (if (>= idx 0)
                 (let [left-part (into (subvec state-small 4 (inc idx)) el3)
                       right-part (subvec state-small (inc idx) (count state-small))
                       new-big (conj state-big (first state-small))]
                   ; (println "A" idx)
                   (recur (into left-part right-part) new-big (inc step))
                 )
                 (let [idx (.indexOf state-big target-el)
                       left-big-part (into (subvec state-big 0 (inc idx)) el3)
                       right-big-part (subvec state-big (inc idx) (count state-big))
                       new-big (conj (into left-big-part right-big-part) (first state-small))]
                   ; (println "SEC" idx)
                   (recur (subvec state-small 4 (count state-small)) new-big (inc step))
                 )
               )
             )
          )
        )
      )
    )
  )

(defn -main
  "Advent of Code. Day 23"
  [& args]
  (let [; inp-string "942387615"
        inp-string "389125467"
        inp-num (into [] (seq (map #(Integer/parseInt (str %)) inp-string)))
        sim-1 (simulate-part1-multi inp-num 100)
        sim-fast-1 (simulate-part1-map inp-num 100)
        result-1 (apply str (map str (drop 1 (rotate-until sim-1 0 1))))]
    (println "Result of part 1:" sim-1 result-1)

    (println "Simulate" sim-fast-1)
    ;; │Wed Dec 30 17:02:49 CET 2020
    ;; Wed Dec 30 17:04:19 CET 2020
    ;; 17:04:51
    (comment
    (let [result-2 (simulate-part1-rotator (into [] (concat inp-num (into [] (range (inc (apply max inp-num)) (- (inc 1000000) (count inp-num)))))) 1000000)
          idx-1 (.indexOf result-2 1)]
      (println result-2)
      (println (nth result-2 idx-1) (nth result-2 (mod (inc idx-1) (count result-2))) (nth result-2 (mod (+ 2 idx-1) (count result-2))))
    )
    )
    )
  )
