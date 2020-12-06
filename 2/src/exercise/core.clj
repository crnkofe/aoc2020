(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])

(defn to_histogram [password]
  (loop [result {}
         word password]
     (if (str/blank? word) 
       result
       (let [word_length (count word)
             remainder (if (> word_length 1) (subs word 1) "")
             ch (nth word 0)]
            (recur (assoc result ch (+ 1 (get result ch 0))) remainder)
          )
       )
    )
  )

(defn to_validator [spec] 
  (try
    (let [[_ min max ch] (re-matches #"(\p{Digit}+)-(\p{Digit}+) (.)" spec)]
      {:min (Integer. min) :max (Integer. max) :ch (nth (name ch) 0)}
      )
    (catch Exception e (println "parsing failure: " (.toString e)))
    )
  )

(defn parse-line [line]
  (let [[validation password] (str/split line #": ")]
      [(to_validator validation) (to_histogram password)]
    )
  )

(defn is-valid [line]
  (let [[validation histogram] (parse-line line)
        count-chars (get histogram (get validation :ch) 0)]
      (and (>= count-chars (get validation :min)) 
           (<= count-chars (get validation :max)))
    )
  )

(defn parse-positions [spec] 
  (try
    (let [[_ min max ch] (re-matches #"(\p{Digit}+)-(\p{Digit}+) (.)" spec)]
      {:first (- (Integer. min) 1) :second (- (Integer. max) 1) :ch (nth (name ch) 0)}
      )
    (catch Exception e (println "parsing failure: " (.toString e)))
    )
  )

(defn parse-line-part2 [line]
  (let [[validation password] (str/split line #": ")]
      [(parse-positions validation) password]
    )
  )

(defn is-valid-part2 [line]
  (let [[validation password] (parse-line-part2 line)
        first-char (nth password (get validation :first))
        second-char (nth password (get validation :second))
        is-first (= first-char (get validation :ch)) 
        is-second (= second-char (get validation :ch))]
       (or (and is-first (not is-second)) (and is-second (not is-first)))
    )
  )


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (clojure.java.io/reader "input")]
    (println "Solution 1:" (count (filter is-valid (line-seq rdr))))
    )
  (with-open [rdr (clojure.java.io/reader "input")]
    (println "Solution 2:" (count (filter is-valid-part2 (line-seq rdr))))
    )
  )
