(ns exercise.core
  (:gen-class))

;; general idea (part 1):
;;   - there will be exactly four squares with (top, left), (top, right), (bottom, left) and (bottom, right)
;;     not having a match
;;   - one idea is for each edge to count occurrences, filter frames with 2 matching edges
;;   - since order is irrelevant I can just multiply those tile id's
;;  the problem with this approach was that tiles can be rotated and mirrored
;;

;; 
;; TODO: Make a simple 4x4 sample for testing
;; TODO: Spis teste za rotacijo, flip

(defn parse-frame [line-lst]
  (let [line-id (first line-lst)
        data (into [] (rest line-lst))]
    {
      :id (Integer/parseInt (peek (re-find #"Tile ([0-9]+):" line-id)))
      :data data
    }
  )
)

(defn load-tile-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq rdr))
  )
)

(defn load-tiles
  "Load tiles from file - tiles are separated by newlines
   Tile format: 
	Tile 2729:
	...#.#.#.#
	####.#....
	..#.#.....
	....#..#.#
	.##..##.#.
	.#.####...
	####.#.#..
	##.####...
	##..#.##..
	#.##...##.
  "
  [filename]
  (loop [data (load-tile-file filename)
		 result []]
	(if (empty? data)
	  result
	  (let [remaining-data (rest (drop-while #(not= "" %) data))
			parsed-frame (parse-frame (take-while #(not= "" %) data))]
		(recur remaining-data (conj result parsed-frame))
	  )
	)
  )
)

(defn generate-edges 
  "creates a list of edges (4 strings) 
   top and bottom are left to right as they are
   left and right are top to bottom"
  [frame]
  (let [data (get frame :data)
        top-edge (first data)
        bottom-edge (peek data)
        left-edge (apply str (map #(first %) data))
        right-edge (apply str (map #(nth % (dec (count %))) data))]
    (list top-edge right-edge bottom-edge left-edge)
  )
)

(defn rotate-list [raw-lst]
  (let [lst (into [] raw-lst)
        temp (concat [(peek lst)] (into [] (drop-last lst)))]
    (list (apply str (reverse (nth temp 0))) 
          (nth temp 1)
          (apply str (reverse (nth temp 2)))
          (nth temp 3))
  )
)

(defn reflect-list [lst]
  (list (nth lst 2) 
        (apply str (reverse (nth lst 1))) 
        (nth lst 0) 
        (apply str (reverse (nth lst 3))))
  )

(defn generate-frame-rotations [frame]
  (let [id (get frame :id)
        d (generate-edges frame)]
    (list {:id id :data d :rot 0 :reflect 0}
          {:id id :data (reflect-list d) :rot 0 :reflect 1}
          {:id id :data (rotate-list d)  :rot 1 :reflect 0}
          {:id id :data (reflect-list (rotate-list d)) :rot 1 :reflect 1}
          {:id id :data (rotate-list (rotate-list d)) :rot 2 :reflect 0}
          {:id id :data (reflect-list (rotate-list (rotate-list d))) :rot 2 :reflect 1}
          {:id id :data (rotate-list (rotate-list (rotate-list d))) :rot 3 :reflect 0}
          {:id id :data (reflect-list (rotate-list (rotate-list (rotate-list d)))) :rot 3 :reflect 1}
    )
  )
)

(defn rotate-frame [lst]
  (let [row-count (count lst)]
    (loop [rotated-frame []
           col 0]
      (if (>= col row-count)
        rotated-frame
        (recur (conj rotated-frame (apply str (reverse (map #(nth % col) lst)))) (inc col))
        )
      )
    )
  )

(defn reflect-frame [lst]
  (reverse lst)
  )

(defn generate-full-frame-rotation [frame rot reflect]
  (let [id (get frame :id)
        d (get frame :data)]
    (case [rot reflect]
      [0 0] {:id id :data d :rot 0 :reflect 0}
      [0 1] {:id id :data (reflect-frame d) :rot rot :reflect reflect}
      [1 0] {:id id :data (rotate-frame d) :rot rot :reflect reflect}
      [1 1] {:id id :data (reflect-frame (rotate-frame d)) :rot rot :reflect reflect}
      [2 0] {:id id :data (rotate-frame (rotate-frame d)) :rot rot :reflect reflect}
      [2 1] {:id id :data (reflect-frame (rotate-frame (rotate-frame d))) :rot rot :reflect reflect}
      [3 0] {:id id :data (rotate-frame (rotate-frame (rotate-frame d))) :rot rot :reflect reflect}
      [3 1] {:id id :data (reflect-frame (rotate-frame (rotate-frame (rotate-frame d)))) :rot rot :reflect reflect}
      )
    )
  )

;; reverting to graph search 
;; trying to do a clean depth-first search here - first I'll just generate all options then from there
;; first generate all available next options going row by row
;; return path of depth first search when reaching last node

(defn filter-horizontal [left-frame frames]
  ;; compare 1st (right side) to 3rd (left)
  (filter #(= (nth (get left-frame :data) 1) (nth (get % :data) 3)) frames)
  )

(defn filter-vertical [left-frame frames]
  ;; compare 1st (right side) to 3rd (left)
  (filter #(= (nth (get left-frame :data) 2) (nth (get % :data) 0)) frames)
  )

(defn dfs-part1-rec
  "Current path is puzzle row by row from left to right"
  [all-options current-path sz]
  (if (empty? all-options)
    (if (= (* sz sz) (count current-path))
      current-path
      nil
      )
    (let [path-ids (into [] (map #(get % :id) current-path))
          next-el-y (quot (count current-path) sz)
          next-el-x (mod (count current-path) sz)]
    (let [current-frame (nth current-path (- (count current-path) 1))
          el-y (quot (- (count current-path) 1) sz)
          el-x (mod (- (count current-path) 1) sz)
          next-y-coord(+ (* sz (- next-el-y 1)) next-el-x)]

      (let [
          filter-horizontal-options (if (= el-y next-el-y) 
                                      (filter-horizontal current-frame all-options) all-options)
          filter-vertical-options (if (> next-el-y 0) 
                                    (filter-vertical (nth current-path next-y-coord) filter-horizontal-options)
                                    filter-horizontal-options)
          ]
      (if (empty? filter-vertical-options)
        nil
        (loop [next-options filter-vertical-options]
          (if (empty? next-options)
            nil
            (let [current-option (first next-options)
                  remaining-options (filter #(not= (get current-option :id) (get % :id)) all-options)]
               (let [result (dfs-part1-rec remaining-options (into [] (conj current-path current-option)) sz)]
                   (if (not (nil? result))
                     result
                     (recur (rest next-options))
                     )
                   )
                 )
             )
          )
       )
    )
    )
    )
  )
)

(defn dfs-part1 [all-frames sz]
  (let [options (flatten (map #(generate-frame-rotations %) all-frames))]
    (loop [tiles options]
      (if (empty? tiles)
        []
        (let [solution (dfs-part1-rec (filter #(not= (get (first tiles) :id) (get % :id)) options) [(first tiles)] sz)]
          (if (not (nil? solution))
            solution
            (recur (rest tiles))
            )
          )
        )
    )
  )
)

(defn cut-edges [piece]
  (into [] (drop 1 (drop-last 1 (map #(apply str (drop 1 (drop-last 1 %))) (into [] piece)))))
  )

(defn reduce-row-piece [piece-data new-piece-data]
  (map #(apply str (nth piece-data %) (nth new-piece-data %)) (range (count piece-data)))
  )

(defn prepare-full-image 
  "Assembly is a list of puzzle blocks (data is unoriented)

  First rows need to be properly rotated and reflected, 
  then each puzzle block needs to be inserted into full image
  "
  [assembly]
  (let [row-width (Math/sqrt (count assembly))
        row-size (count (get (nth assembly 0) :data))]
    (loop [full-image []
            row 0]
        (if (>= row row-width)
          full-image
          (let [pieces (map #(cut-edges 
                               (get (nth assembly %) :data)) 
                            (range (* row row-width) (* (inc row) row-width)))
                merged-pieces (reduce reduce-row-piece (first pieces) (rest pieces))
                ]
            (recur (concat full-image merged-pieces) (inc row))
            )
          )
        )
      )
  )

(defn is-monster-part [candidate-row nessie-row]
  (if (= (count candidate-row) (count nessie-row))
    (let [nessie-idx-hash (map first (filter #(not= (second %) \ ) (map-indexed vector nessie-row)))
          hashes (count (filter #(= \# (nth candidate-row % nil)) nessie-idx-hash))
          non-hashes (count (filter #(not= \# (nth candidate-row % nil)) nessie-idx-hash))
          nessie-idx-empty (map first (filter #(= (second %) \ ) (map-indexed vector nessie-row)))
          empty-hashes (count (filter #(= \# (nth candidate-row % nil)) nessie-idx-empty)) ]
      [(and (> hashes 0) (= non-hashes 0)) empty-hashes]
    )
    [false 0]
  )
)

(defn is-monster [candidate nessie]
  (if (= (count candidate) (count nessie))
    (let [row-results (map #(is-monster-part (nth candidate % "") (nth nessie % "")) (range (count nessie)))
          is-match (if (> (count row-results) 0) (= 0 (count (filter #(= false (first %)) row-results))) false)
          count-common (if (> (count row-results) 0) (reduce + (map second row-results)) 0)]
      [is-match count-common]
    )
    [false 0]
  )
)

(defn find-monsters [full-image]
  (let [nessie ["                  # " 
                "#    ##    ##    ###" 
                " #  #  #  #  #  #   "]
        nessie-hash-power (count (filter #(= \# %) (flatten (map #(into [] %) nessie))))
        ness-size (count (first nessie))
        row-len (count (first full-image))
        row-count (count full-image)]
   (loop [row 0
          col 0
          total-count 0]
     (if (> row row-count)
       total-count
       (let [next-row (if (= 0 (mod (inc col) row-len)) (inc row) row)
             candidate-monster (map #(apply str %) [(take ness-size (drop col (nth full-image row "")))
                                (take ness-size (drop col (nth full-image (+ 1 row) "")))
                                (take ness-size (drop col (nth full-image (+ 2 row) "")))])
             [is-match count-matching] (is-monster candidate-monster nessie)]
           (recur next-row (mod (inc col) row-len) (if is-match (+ nessie-hash-power total-count) total-count))
         )
       )
     )
   )
 )

(defn -main
  "Advent of Code 2020. Day 20"
  [& args]
  (let [filename "input.txt"
        loaded-tiles (load-tiles filename) 
        map-size (int (Math/round (Math/sqrt (count loaded-tiles))))
        assembly (dfs-part1 loaded-tiles map-size)]
    (let [corners (list (nth assembly 0) 
                        (nth assembly (dec map-size))
                        (nth assembly (- (* map-size map-size) map-size))
                        (peek assembly))]
      (println "Result of part 1:" (reduce * (map #(get % :id) corners)))
      )

    (let [tile-map (reduce (fn [tile-map tile] (assoc tile-map (get tile :id) tile)) {} loaded-tiles)
          full-assembly (map #(generate-full-frame-rotation (get tile-map (get % :id)) (get % :rot) (get % :reflect)) assembly)
          full-image (prepare-full-image full-assembly) ]
      (loop [combos [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1] [3 0] [3 1]]]
        (if (empty? combos)
          nil
          (let [frame-variant (generate-full-frame-rotation {:id 1 :data full-image} (first (first combos)) (second (first combos)))
                monster-count (find-monsters (get frame-variant :data))
                total-hash-count (count (filter #(= \# %) (apply concat (flatten (into [] (get frame-variant :data))))))]
            (when (> monster-count 0)
              ;; it turns out only one habitat contains monsters
              (println "Result of part 2:" (- total-hash-count monster-count))
              )
            (recur (rest combos))
           )
          )
        )
      )
    )
  )
