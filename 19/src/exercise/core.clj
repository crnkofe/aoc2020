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
        {:id idx :rule (nth (re-find #"\"(.*)\"" line) 1)}
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
    (reduce conj []  (drop-while #(not= % "") (line-seq rdr)))
  )
)

(defn -main
  "Advent of Code. Day 19"
  [& args]
  (let [filename "sample.txt"
        rules (load-rules filename)
        strs (load-strings filename)
        ]
    (println "rules" rules)
    (println "strs" strs)
  )
)
