(ns exercise.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])


(defn load-passports [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [lines (flatten (reduce conj [] (map #(str/split % #" ") (line-seq rdr))))
          parsed-lines (map #(str/split % #":") lines)
          partitioned-list (partition-by #(= 1 (count %)) parsed-lines)
          non-empty-list (filter #(> (count %) 1) partitioned-list)
          ]
          (map #(into {} %) non-empty-list)
        )
      )
  )

(defn valid-passport [passports] 
  (filter #(or (= 8 (count %)) (and (= 7 (count %)) (not (contains? % "cid")))) passports)
  )

(defn digit-check [passport key min-value max-value]
  (try
    (let [value (Integer. (re-find #"[0-9]*" (get passport key "")))]
        (and (>= value min-value) (<= value max-value))
      )
    (catch Exception e 
      false)
    )
  )

(defn height-check [passport]
  (try
    (if (.endsWith (get passport "hgt") "cm")
      (let [number (Integer. (first (str/split (get passport "hgt") #"cm")))]
        (and (>= number 150) (<= number 193))
        )
      (let [number (Integer. (first (str/split (get passport "hgt") #"in")))]
        (and (>= number 59) (<= number 76))
        )
      )
    (catch Exception e 
      false)
    )
  )

(defn hair-check [passport]
  (let [value (re-find #"#[0-9a-f]{6}" (get passport "hcl" ""))]
    (not (nil? value))
    )
  )

(defn eye-color-check [passport]
  (let [color (get passport "ecl" "")
        valid-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}]
    (contains? valid-colors color)
    )
  )

(defn pid-check [passport]
  (let [value (re-find #"^[0-9]{9}$" (get passport "pid" ""))]
    (not (nil? value))
    )
  )


(defn valid-passport-part2 [passports]
  (let [almost-valid-passports (filter #(or (= 8 (count %)) (and (= 7 (count %)) (not (contains? % "cid")))) passports)
        byr-check (filter #(digit-check % "byr" 1920 2002) almost-valid-passports)
        iyr-check (filter #(digit-check % "iyr" 2010 2020) byr-check)
        eyr-check (filter #(digit-check % "eyr" 2020 2030) iyr-check)
        hgt-check (filter height-check eyr-check)
        hcl-check (filter hair-check hgt-check)
        eye-check (filter eye-color-check hcl-check)
        result (filter pid-check eye-check)]
      result
    )
  )

(defn -main
  "Advent of code '20, 4. star"
  [& args]
  (let [passports (load-passports "input.txt")]
   (println "Valid passport count 1" (count (valid-passport passports)))
   (println "Valid passport count 2" (count (map #(get % "pid") (valid-passport-part2 passports))))
    )
)
