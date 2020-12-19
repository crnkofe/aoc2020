(ns exercise.core
  (:gen-class))

(defn load-rules
  "Load ruleset"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (take-while #(not= % "") (line-seq rdr)))
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
