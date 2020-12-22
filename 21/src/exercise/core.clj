(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set :as sets])

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

(defn find-non-alergens
  "The idea is that simply organize most frequent to least frequent allergen
  take all assignments with that allergen - do set intersections between them - what remains must be an alergen
  remove the alergen and ingredient and repeat
  "
  [assignments]

  (let [freqs (into [] (reverse (sort-by #(nth % 1) (frequencies (reduce concat [] (map #(get % :allergens) assignments))))))]
    (loop [remaining-freqs freqs
           assign assignments]
        (if (empty? remaining-freqs)
          assign
          (let [[allergen _] (first remaining-freqs)
                result-ingredient (first (reduce sets/intersection (map #(get % :ingredients) (filter #(contains? (get % :allergens) allergen) assign))))
                new-assign (map #(hash-map :ingredients (disj (get % :ingredients) result-ingredient) :allergens (disj (get % :allergens) allergen)) assign)]
            (recur (rest remaining-freqs) new-assign)
          )
        )
      )
    )
  )

(defn count-appearances [parsed-input]
  (loop [filter-ingredients (reduce sets/union (map #(get % :ingredients) (find-non-alergens parsed-input)))
         count-appearances 0]
      (if (empty? filter-ingredients)
        count-appearances
        (recur (rest filter-ingredients) (+ count-appearances (count (filter #(contains? (get % :ingredients) (first filter-ingredients)) parsed-input))))
        )
    )
  )

(defn find-alergens
  "The idea is that simply organize most frequent to least frequent allergen
  take all assignments with that allergen - do set intersections between them - what remains must be an alergen
  remove the alergen and ingredient and repeat
  "
  [assignments]

  (let [freqs (into [] (reverse (sort-by #(nth % 1) (frequencies (reduce concat [] (map #(get % :allergens) assignments))))))]
    (loop [remaining-freqs freqs
           assign assignments
           allergen-map {}]
        (if (empty? remaining-freqs)
          allergen-map
          (let [[allergen _] (first remaining-freqs)
                result-ingredient (first (reduce sets/intersection (map #(get % :ingredients) (filter #(contains? (get % :allergens) allergen) assign))))
                new-assign (map #(hash-map :ingredients (disj (get % :ingredients) result-ingredient) :allergens (disj (get % :allergens) allergen)) assign)]
            (recur (rest remaining-freqs) new-assign (assoc allergen-map allergen result-ingredient))
          )
        )
      )
    )
  )

(defn -main
  "Advent of Code. Day 21"
  [& args]
  (let [filename "input.txt"
        parsed-input (parse-input filename)
        all-alergens (reduce sets/union #{} (map #(get % :allergens) parsed-input))
        all-ingredients (reduce sets/union #{} (map #(get % :ingredients) parsed-input))
        ]
    (println "Solution of part 1:" (count-appearances parsed-input))
    (println "Solution of part 2:" (str/join "," (map #(nth % 1) (sort-by #(nth % 0) (find-alergens parsed-input)))))

  )
)
