(ns exercise.core
  (:gen-class))

(require '[clojure.pprint :as pprint])
(require '[clojure.string :as str])

(defn parse-condition 
  "Parse a line of form xyz: 1-3 or 5-7"
  [s]
  (let [result (re-find #"(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" s) 
        [_ name low-min low-max high-min high-max] result]
    {
      :name name
      :low [(Integer/parseInt low-min) (Integer/parseInt low-max)]
      :high [(Integer/parseInt high-min) (Integer/parseInt high-max)]
    }
  )
)

(defn parse-ticket-data
  "Parse lines of form X,Y,Z where each X,Y,Z is a number"
  [s]
  (into [] (map #(Integer/parseInt %) (str/split s #",")))
)

(defn load-tickets
  "Parse file of form 
  x: 1-3 or 5-7
  y: 1-5...
  ...
  
  your ticket:
  7,1,14
  
  nearby tickets:
  7,3,47
  40,4,50
  ...  "
  [filename]                        
  (let [lines (with-open [rdr (clojure.java.io/reader filename)]                                                                                                                          
                (reduce conj [] (line-seq rdr))
              )]
    (loop [mode :ticket
           remaining-lines (filter #(not (empty? %)) lines)
           ticket-info {:fields [] :my-ticket [] :nearby-tickets []}]
      (if (empty? remaining-lines)
        ticket-info
        (let [current-line (first remaining-lines)
              new-mode (if (.startsWith current-line "your ticket") :my-ticket 
                           (if (.startsWith current-line "nearby tickets") :nearby-ticket mode))]
          (if (= mode new-mode)
            (let [new-ticket-info  
              (case mode
                :ticket (assoc ticket-info :fields (conj (get ticket-info :fields) (parse-condition current-line)))
                :my-ticket (assoc ticket-info :my-ticket (conj (get ticket-info :my-ticket) (parse-ticket-data current-line)))
                :nearby-ticket (assoc ticket-info :nearby-tickets (conj (get ticket-info :nearby-tickets) (parse-ticket-data current-line)))
              )]
              (recur new-mode (rest remaining-lines) new-ticket-info)
            )
            (recur new-mode (rest remaining-lines) ticket-info)
          )
        )
      )
    )
  )
)

(defn between [range number]
  (and (>= number (first range)) (<= number (nth range 1)))
)

(defn is-field-match [field value]
  (or (between (get field :low) value) (between (get field :high) value))
  )

(defn filter-matching-fields [fields value]
  (filter #(is-field-match % value) fields)
  )

(defn filter-matching-values [field values]
  (let [matching-values (filter #(is-field-match field %) values)]
    (= (count values) (count matching-values))
  )
)


(defn verify-nearby 
  "for each nearby ticket verify that each number falls within one of fields"
  [ticket-info]
  (let [nearby-tickets (flatten (get ticket-info :nearby-tickets))]
    (filter #(empty? (filter-matching-fields (get ticket-info :fields) %)) nearby-tickets)
  )
)

(defn filter-nearby 
  "for each nearby ticket verify that each number falls within one of fields - return valid tickets only"
  [fields nearby-tickets valid-tickets]
  (if (empty? nearby-tickets)
    valid-tickets 
    (let [values (flatten (first nearby-tickets)) 
          matching-values (filter #(empty? (filter-matching-fields fields %)) values)]
      (if (empty? matching-values)
        (filter-nearby fields (rest nearby-tickets) (conj valid-tickets (first nearby-tickets)))
        (filter-nearby fields (rest nearby-tickets) valid-tickets)
      )
    )
  )
)

(defn invert [fields inverted idx ct]
  (if (>= idx ct)
    inverted
    (invert fields (assoc inverted idx (into [] (map #(nth % idx) fields))) (inc idx) ct)
  )
)

(defn find-field-index
  "for each field find the numeric column, for which all entries fall on the same field"
  [ticket-info]
  (let [valid-tickets (filter-nearby (get ticket-info :fields) (get ticket-info :nearby-tickets) [])
        inverted (invert valid-tickets {} 0 (count (first (get ticket-info :my-ticket))))
        field-count (count (first (get ticket-info :my-ticket)))]
    (loop [fields (get ticket-info :fields)
           field-index {}]
      (if (empty? fields)
        field-index
        (let [current-field (first fields)
              options (filter #(filter-matching-values current-field (get inverted %)) (range field-count))]
            (recur (rest fields) (assoc field-index (get current-field :name) options)) 
          )
        )
      )
    )
  )

(defn determine-index 
  "At each iteration filter map by filterting all values by mapped indices, 
   then taking the map with a single entry (result)"
  [m]
  (loop [idx 1
         field-indices {}]
    (if (>= idx (count m))
      field-indices
      (let [entries (filter #(= idx (count (nth % 1))) m)
            filtered-entries (into [] (filter #(not (contains? (into #{} (vals field-indices)) %)) (flatten entries)))]
        (recur (inc idx) (assoc field-indices (str/join " " (drop-last (first entries))) (peek filtered-entries)))
      )
    )   
  )
)

(defn -main
  "Advent of Code 2020. Day 16"
  [& args]
  (let [ticket-info (load-tickets "input.txt")
        all-field-indices (find-field-index ticket-info)
        field-indices (determine-index all-field-indices)
        ticket-indices (map #(nth % 1) (filter #(.startsWith (nth % 0) "departure") field-indices))]
    (println "Solution of part 1:" (reduce + (verify-nearby ticket-info)))
    (println "Solution of part 2:" (reduce * (map #(nth (first (get ticket-info :my-ticket)) %) ticket-indices)))
  )
)
