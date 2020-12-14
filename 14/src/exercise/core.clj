(ns exercise.core
  (:gen-class))

(defn parse-mask 
  "Assume line is of form XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
   bitmask starts as zero and is returned as bitmask at the end
   count is current 2^count bit
   Should be called as (parse-mask \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\" 0 0)"
  [line ormask andmask count]
  (if (empty? line)
    [ormask andmask]
    (if (= \1 (first line))
      (parse-mask (rest line) (+ 1 (* 2 ormask)) (* 2 andmask) (inc count))
      (if (= \0 (first line))
        (parse-mask (rest line) (* 2 ormask) (+ 1 ( * 2 andmask)) (inc count))
        (parse-mask (rest line) (* 2 ormask) ( * 2 andmask) (inc count))
      )
    )
  )
)

(defn load-commands
  "Parses file to instructions
   mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
   mem[8] = 11"
  [filename]                        
  (let [lines (with-open [rdr (clojure.java.io/reader filename)]                                                                                                                          
                (reduce conj [] (line-seq rdr))
              )]
    (loop [unprocessed-lines lines
           processed-lines []
           current-mask nil
           memory {}]
        (if (empty? unprocessed-lines)
          memory
          (let [line (first unprocessed-lines)
                parsed-mask (re-find #"mask = (.*)" line)
                parsed-address (re-find #"mem\[([0-9]+)\] = ([0-9]+)" line)
                new-mask (if (nil? (nth parsed-mask 1)) current-mask (parse-mask (nth parsed-mask 1) 0 0 0))
                location (nth parsed-address 1)
                value (nth parsed-address 2)
]
            (recur 
              (rest unprocessed-lines) 
              processed-lines 
              new-mask 
              (if (not (nil? location))
                (assoc memory (Integer/parseInt location) (bit-and (bit-or (Integer/parseInt value) (nth current-mask 0)) (bit-not (nth current-mask 1))))
                memory
              ))
          )
        )
    )
  )
)

(defn reduce-map-address 
  "Associate value to map at address"
  [value map address]
  (assoc map address value)
)

(defn generate-addresses
  "Assume line is of form XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
   Generate addresses generates all possible representations from mask (string) and given address (number)
   
   Every X can be represented by either 0 or 1.
   line should be passed in reversed (starts from lowest bits and continues to end)
   number starts as 0 and in the end becomes the result
   If element of mask is X fuction generates two numbers which get merged at the end
   nth-pot gets multiplied by 2 and represents current power of 2 that is being processed for ease of number manipulation
  "
  [line number address nth-pot]
  (if (empty? line)
    [number]
    (case (first line)
      \X (let [option1 (bit-or number nth-pot)
               option2 (bit-and number (bit-not nth-pot))]
         (list
           (generate-addresses (rest line) option1 address (* 2 nth-pot))
           (generate-addresses (rest line) option2 address (* 2 nth-pot))
         )
      )
      \0 (let[] 
        (generate-addresses (rest line) (bit-or number (bit-and address nth-pot)) address (* 2 nth-pot))
        
     )
      \1 (generate-addresses (rest line) (bit-or number nth-pot) address (* 2 nth-pot))
      (let [] 
        []
      )
    )
  )
)

(defn exec-commands-part2
  "Parses file to instructions
   mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
   mem[8] = 11 

  Apply part 2 logic where we generate addresses and write value to each generated address.
  "
  [filename]                        
  (let [lines (with-open [rdr (clojure.java.io/reader filename)]                                                                                                                          
                (reduce conj [] (line-seq rdr))
              )]
    (loop [unprocessed-lines lines
           processed-lines []
           current-mask nil
           memory {}]
        ; (println (first unprocessed-lines))
        (if (empty? unprocessed-lines)
          memory
          (let [line (first unprocessed-lines)
                parsed-mask (re-find #"mask = (.*)" line)
                parsed-address (re-find #"mem\[([0-9]+)\] = ([0-9]+)" line)
                location (nth parsed-address 1)
                value (nth parsed-address 2)]
            (recur 
              (rest unprocessed-lines) 
              processed-lines 
              (if (nil? (nth parsed-mask 1)) current-mask (nth parsed-mask 1))
              (if (not (nil? location))
                (let [addresses (flatten (generate-addresses (reverse current-mask) 0 (Integer/parseInt location) 1))]
                  ; (println "a" location value current-mask addresses)
                  (reduce (partial reduce-map-address (Integer/parseInt value)) memory addresses)
                )
                memory
              )
          )
        )
      )
    )
  )
)

(defn -main
  "Advent of Code. Day 14"
  [& args]
  ; (println "Result of part 1" (reduce + (vals (load-commands "sample.txt"))))
  ; (println (flatten (generate-addresses (reverse "000000000000000000000000000000X1001X") 0 42 1)))
  ; (println (flatten (generate-addresses (reverse "00000000000000000000000000000000X0XX") 0 26 1)))
  (println "REsult of part 2" (reduce + (vals (exec-commands-part2 "input.txt"))))
)
