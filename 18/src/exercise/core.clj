(ns exercise.core
  (:gen-class))

(defn parse-operator [ch]
  (case ch
    \+ +
    \* *
    \/ /
    \- -
    nil
    )
  )

(defn parse-number [ch previous-number]
  (if (not (Character/isDigit ch))
    nil
    (try
      (+ (Character/digit ch 10) (* previous-number 10))
    (catch Exception e ;; ch is not a number
      nil)
    )
  )
)

(defn collapse-stack-once [number-stack operator]
  (if (>= (count number-stack) 2) 
    [(apply operator number-stack)]
     number-stack
  )
)

(defn eval-expression-wo-precedence 
  "operator always evaluates left to right ignoring * / precedence
   expressions can have braces which do work as usual"
  [char-index raw-line]
  (let [line (filter #(not= \  %) (into [] raw-line))]
    (loop [char-index char-index
           previous-num 0
           number-stack [] ;; each time a number is fully parsed
           previous-operator nil]
      (if (or (>= char-index (count line)) (= \) (nth line char-index)))
        (let [result (first (collapse-stack-once (conj number-stack previous-num) previous-operator))]
          [result char-index]
        )
        (let [num (parse-number (nth line char-index) previous-num)
              operator (parse-operator (nth line char-index))
              new-number-stack (if (not (nil? operator)) (conj number-stack previous-num) number-stack) 
              applied-number-stack (collapse-stack-once new-number-stack previous-operator)]
          (if (= \( (nth line char-index) )
            (let [[result new-idx] (eval-expression-wo-precedence (inc char-index) line)]
              (recur (inc new-idx)
                     result
                     applied-number-stack
                     previous-operator)
            )
            (recur (inc char-index) 
                   (if (not (nil? num)) num 0)
                   applied-number-stack
                   (if (nil? operator) previous-operator operator))
            )
         )
      )
    )
  )
)

(defn next-token 
  "Parses next full token (operator (*,-,/,+) or full number or (,)
   Returns [token next-idx]
   "
  [line idx]
  (if (or (nil? idx) (>= idx (count line)))
    nil
    (case (nth line idx)
      \* [\* (inc idx)]
      \+ [\+ (inc idx)]
      \- [\- (inc idx)]
      \/ [\/ (inc idx)]
      \( [\( (inc idx)]
      \) [\) (inc idx)]
      (loop [number 0
             current-idx idx]
        (if (>= current-idx (count line))
          [number (inc current-idx)]
          (let [new-number (parse-number (nth line current-idx) number)]
            (if (nil? new-number)
              [number current-idx]
              (recur new-number (inc current-idx))
            )
          )
        )
      )
    )
  )
)

(defn coalesce
  "Returns first non-nil argument."
  [& args]
    (first (keep identity args)))

(defn precedence [op]
  (case op
    \( 100
    \) 100
    \+ 10
    \- 10
    \* 1
    \/ 1
    0
    )
  )

(defn collapse-full-stack [stack]
  (if (>= (count stack) 3)
    (let [last-idx (dec (count stack))
          operator (nth stack (dec last-idx))]
      (let [result (into [] (case operator
        \* (concat (drop-last 3 stack) [(* (nth stack (- last-idx 2)) (nth stack last-idx))] )
        \- (concat (drop-last 3 stack) [(- (nth stack (- last-idx 2)) (nth stack last-idx))] )
        \+ (concat (drop-last 3 stack) [(+ (nth stack (- last-idx 2)) (nth stack last-idx))] )
        \/ (concat (drop-last 3 stack) [(/ (nth stack (- last-idx 2)) (nth stack last-idx))] )
      ))]
        result
        )
    )
    stack
  )
)

(defn collapse-all [stack]
  (if (= (count stack) 1)
    stack
    (collapse-all (collapse-full-stack stack))
  )
)

(defn eval-expression-w-wrong-precedence
  "Advent of Code 17 Part II
   operator always evaluates left to right and makes + have higher precedence than multiplication
   expressions can have braces which do work as usual"
  [start-index raw-line]
  (let [line (filter #(not= \  %) (into [] raw-line)) ]
    (loop [[token next-idx] (coalesce (next-token line start-index))
           stack []]
        (let [[after-token post-idx] (coalesce (next-token line next-idx))]
          (if (or (nil? token) (= token \)))
            [next-idx (first (collapse-all stack))]
            (let [previous-token (if (not (empty? stack)) (peek stack) nil)]
              (if (= token \()
                (let [[idx result] (eval-expression-w-wrong-precedence next-idx raw-line) 
                      after-token (coalesce (next-token line idx) [nil nil]) 
                      should-collapse (> (precedence previous-token) (precedence (first after-token)))]
                   (recur after-token (if should-collapse (collapse-all (conj stack result)) (conj stack result)))
                  )
                (if (and (number? token) (>= (precedence after-token) (precedence previous-token)))
                  (recur [after-token post-idx] (conj stack token))
                  (recur [after-token post-idx] (if (number? token) (collapse-all (conj stack token)) (conj stack token))) ;; eval stack here
                  )
              )
            )
        )
      )
    )
  )
)

(defn calculate-expressions                                                       
  "evaluate each line as an expression and return results "
 [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map (partial eval-expression-wo-precedence 0) (line-seq rdr)))
  )    
)


(defn calculate-expressions-part2
  "evaluate each line as an expression and return results "
 [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (map (partial eval-expression-w-wrong-precedence 0) (line-seq rdr)))
  )    
)

(defn -main
  "Advent of Code. Day 18"
  [& args]
  (let [evaluated-expressions (calculate-expressions "input.txt")
        evaluated-expressions-part2 (calculate-expressions-part2 "input.txt")]
   (println "Solution of part 1:" (reduce + (map #(nth % 0) evaluated-expressions)))
   (println "Solution of part 2:" (reduce + (map #(nth % 1) evaluated-expressions-part2)))
   )
)
