(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])

(defn lst-as-int [lst] 
  (into [] (map #(Integer/parseInt %) (filter #(not (empty? %)) (str/split lst #" "))))
)

(defn parse-rule [line]
  (let [[_ idx-raw content] (re-find #"([0-9]+): (.*)" line)
        idx (Integer/parseInt idx-raw)]
      (if (.contains content "\"")
        {:id idx :rule (first (nth (re-find #"\"(.*)\"" line) 1))}
        {:id idx :or (into [] (map #(lst-as-int %) (str/split content #"\|")))}
      )
  )
)

(defn load-rules
  "Load ruleset"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map parse-rule (take-while #(not= % "") (line-seq rdr))))
  )
)

(defn load-strings [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (rest (reduce conj [] (drop-while #(not= % "") (line-seq rdr))))
  )
)

(defn validate-char [s rule char-idx]
  (if (>= char-idx (count s))
    [false char-idx]
    (let []
      [(= (nth s char-idx) (get rule :rule)) (inc char-idx)]
    )
  )
)

(declare validate-indices)

(defn check-rule [s offset rule rules limit]
  (if (<= limit 0)
    (let [] 
       (println "limit reached")
    [[false offset]]
    )
	(if (contains? rule :rule)
	  (let [[valid next-idx] (validate-char s rule offset)]
        (println "check-rule-valid" valid rule offset (count s))
		(if (not valid)
		  [[false next-idx]]
		  [[true next-idx]]
		)
	  )
	  (let [] 
	  ; (println "check-rule" (get rule :id))
	  (let [[valid-left left-idx] (validate-indices s (first (get rule :or)) rules offset (dec limit) [])
            right-result (if (> (count (get rule :or)) 1) (validate-indices s (nth (get rule :or) 1) rules offset (dec limit) []) []) 
            result (into [] (concat [ [valid-left left-idx] ] [right-result]))]
		(println "result" result)
		result
	  )
	  )
	)
  )
)

(defn validate-indices
  "Validates rules of form [n1 n2 n3]}
   Rules must be validated in order so when first is validated go to nth char index and continue"
  [s indices rules offset limit path]
  (if (<= limit 0)
    (let [] 
      (println "limit reached" limit)
      [false offset]
    )
    ; (println indices (empty? indices))
    (if (empty? indices)
      (let []
        (println "end-of-the-line-0" indices offset path)
        [true offset path] ;; rules must match entire string
      )
    ; (println "check" (first indices))
      (let [next-rule (first (filter #(= (first indices) (get % :id)) rules)) 
            results (filter #(= true (first %)) (check-rule s offset next-rule rules (dec limit)))]
          ;; TODO: make this work at each offset
          (if (empty? results)
            (let []
              (println "end-of-the-line-1" indices offset path)
              [false offset path]
            )
            (let [validated-results (map #(validate-indices s (rest indices) rules (nth % 1) (dec limit) (conj path (nth % 1))) results)
                  filtered-results (filter #(= true (first %)) validated-results)]
              (if (empty? filtered-results)
                (let []
                  (println "results" validated-results)
                  (println "hakuna" indices offset)
                  [false offset path]
                )
                (let []
                  (println "filtered-results" indices filtered-results)
                  (first filtered-results)
                )
              )
            )
          )
      )
    )
    )
  )

(defn validate-string [s all-rules]
  (let [rule-0 (first (filter #(= 0 (get % :id)) all-rules))
        [result final-offset path] (validate-indices s (first (get rule-0 :or)) all-rules 0 1000 [])]
    (println "AJAJA" result final-offset path)
    (and result (>= final-offset (count s)))
  )
)

(defn -main
  "Advent of Code. Day 19"
  [& args]
  (let [filename "sample3.txt"
        rules (load-rules filename)
        strs (load-strings filename)
        ]
    ; (println "Solution of part 1:" (count (filter #(validate-string % rules) strs)))
    (println "Solution of part 2:" (filter #(validate-string % rules) strs))
  )
)
