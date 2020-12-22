(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set :as sets])

;; idea 1:
;; - start by parsing input, for each token get all possible alergens (store all alergens as set) it can contain (contains only one)
;; - idea start parsing with the shortest list (least ideas to try early one)
;; - do the following recursively - assign allergen to one token
;; - try removing it and the allergenm from every list - if in any list only 1 token remains it gets automatically assigned
;; - if the ingredient is the only one in list check if the list

(defn parse-line [line]
  (let [[ingredients-raw allergens-raw] (str/split (apply str (drop-last line)) #" \(contains ")]
      {:ingredients (into #{} (str/split ingredients-raw #" "))
       :allergens (into #{} (str/split allergens-raw #", "))}
    )
  )

(defn parse-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map #(parse-line %) (line-seq rdr)))
    )
  )

(defn coalesce
  "Returns first non-nil argument."
  [& args]
  (first (keep identity args)))

(defn assign-single-allergen [assignments allergen ingredient]
  (map #(hash-map :ingredients (get % :ingredients) :allergens (disj (get % :allergens) allergen)) assignments)
  )

(defn assign-allergens
  "assigns alergens depth first - removes ingredient and alergen from all assignments - then checks validity
   assignments is 
   all-ingredients is a set of all ingredients"
  [assignments all-allergens assigned]
  ; (println "ass" assignments all-allergens assigned)
  (if (empty? all-allergens)
    [assignments assigned]
    (loop [allergen-id 0]
      ; (println "allergen-id" allergen-id (map #(get %  :ingredients) assignments))
      (if (>= allergen-id (count all-allergens))
        [nil nil]
        (let [allergen-lst (into [] all-allergens)
              allergen (nth allergen-lst allergen-id)
              remaining-ingredients (reduce sets/union #{} (map #(get % :ingredients) assignments))
              result (first (filter #(not= [nil nil] 
                                   (assign-allergens 
                                     (assign-single-allergen assignments allergen %)
                                     (disj all-allergens allergen)
                                     (merge assigned {allergen %})
                                   )) remaining-ingredients ))]
            (if (not= [nil nil] result)
              result
              (recur (inc allergen-id))
              )
          )
        )
      )
    )
  )

(defn -main
  "Advent of Code. Day 21"
  [& args]
  (let [filename "sample.txt"
        parsed-input (parse-input filename)
        all-alergens (reduce sets/union #{} (map #(get % :allergens) parsed-input))
        all-ingredients (reduce sets/union #{} (map #(get % :ingredients) parsed-input))]
    (println parsed-input)
    (println (assign-allergens parsed-input all-alergens {}))
  )
)
