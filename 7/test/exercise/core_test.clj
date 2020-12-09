(ns exercise.core-test
  (:require [clojure.test :refer :all]
            [exercise.core :refer :all]))

(deftest parse-line-test
  (testing "Should parse line."
    (let [result (parse-line "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.")]
      (println result)
      (is (= [["shiny gold" "dark olive"] ["shiny gold" "vibrant plum"]] result)))
    )
  )

(deftest parse-empty-test
  (testing "Should parse line."
    (is (= []  (parse-line "faded blue bags contain no other bags."))
        )
    )
  )

(deftest parse-single-bag-test
  (testing "Should parse single bag instruction"
    (is (= {:node "faded blue" :count 5} (parse-single-bag "5 faded blue"))
        )
    )
  )
