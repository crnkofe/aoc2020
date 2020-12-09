(ns exercise.core
  (:gen-class))


(require '[clojure.set :as set])
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

; "load file - generate all parent child mappings" 
; "filter all shiny gold bags and find all parents that are reachable from shiny gold bags" 

; part 2 - I need to load graph counts as well as nodes
; then the exercise is simple - starting from all shiny 
; gold roots traverse depth first and calculate amount of bags that need to be contained

(defn parse-line  
  "Generate a list of lists [[node child]]
   sample: 'from shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.'"
  [line]
    (if (.contains line " contain no other bags")
      []
      (let [cleanup (str/replace (str/replace (str/replace (str/replace (str/replace line #"from " "") #" bags\." "") #" bag\." "") #" bags" "") #" bag" "")
            split-data (str/split cleanup #" contain ")]
        (if (<= (count split-data) 1)
          (let []
            [])
          (
           let [node (first split-data)
                raw-children (str/split (nth split-data 1) #", ")
                result (map #(list node (nth (str/split % #" " 2) 1)) raw-children)
                ]
              (into [] result)
             )
        )
      )
    )
  )

(defn parse-single-bag 
  "Expects a string of form '1 color name' and returns a map {:node 'color name' :count 1}"
  [description]
  (let [[ct name] (str/split description #" " 2)]
	{:node name :count (Integer/parseInt ct)}
  )
)

(defn parse-bags
  "Generate a list of lists [[node child]]
   sample: 'from shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.'"
  [line]
    (if (.contains line " contain no other bags")
      []
      (let [cleanup (str/replace (str/replace (str/replace (str/replace (str/replace line #"from " "") #" bags\." "") #" bag\." "") #" bags" "") #" bag" "")
            split-data (str/split cleanup #" contain ")]
        (if (<= (count split-data) 1)
          (let []
            [])
          (
           let [node (first split-data)
                children (map parse-single-bag (str/split (nth split-data 1) #", "))
                result (map #(list {:node node} %) children)
                ]
              (into [] result)
             )
        )
      )
    )
  )

(defn load-baggage-rules [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce concat [] (map #(parse-line %) (line-seq rdr)))
    )
  )

(defn load-full-baggage-rules [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce concat [] (map parse-bags (line-seq rdr)))
    )
  )

(defn find-remaining [tree reached]
  (let [filtered-end-pairs (filter #(contains? reached (nth % 1)) tree)
        filtered-end-nodes (into #{} (map #(nth % 1) filtered-end-pairs))
        filtered-parents (into #{} (map #(nth % 0) filtered-end-pairs))
        remaining-end-nodes (filter #(not (contains? reached (nth % 1))) tree)
		new-reached (set/union reached filtered-parents)]
    (if (empty? filtered-end-nodes)
       reached
       (let []
         (find-remaining remaining-end-nodes new-reached))
      )
    )
  )

(defn find-reachable [tree]
  (let [valid-end-pairs (filter #(= (nth % 1) "shiny gold") tree)
		valid-end-nodes (into #{} (map #(nth % 1) valid-end-pairs))
		all-nodes (set/union (into #{} (map #(nth % 0) tree)) (into #{} (map #(nth % 1) tree)))]
    (find-remaining tree valid-end-nodes)
    )
  )

(defn traverse-bags-first [node tree]
  (let [matching-pairs (filter #(= node (get (nth % 0) :node)) tree)
        pairs (map #(nth % 1) matching-pairs)]
    (if (empty? matching-pairs)
      0
      (let [result (reduce + (map #(* (get % :count) (traverse-bags-first (get % :node) tree)) pairs))
            count-sum (reduce + (map #(get % :count) pairs))]
        (+ count-sum result)
       )
    )
  )
)

(defn -main
  "Advent of Code 2020 - Exercise 7"
  [& args]
  (let [baggage-rules (load-baggage-rules "input.txt")
        bag-rules (load-full-baggage-rules "input.txt")]
  (println "Part 1:" (- (count (find-reachable baggage-rules)) 1))
  (println "Part 2:" (traverse-bags-first "shiny gold" bag-rules))
  )
)
