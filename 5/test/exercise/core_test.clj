(ns exercise.core-test
  (:require [clojure.test :refer :all]
            [exercise.core :refer :all]))

(deftest parse-row-1
  (testing "row 1 index test"
    (is (= 357 (parse-line "FBFBBFFRLR"))))
  )


(deftest parse-row-2
  (testing "row 2 index test"
    (is (= 567 (parse-line "BFFFBBFRRR"))))
  )

(deftest parse-row-3
  (testing "row 3 index test"
    (is (= 119 (parse-line "FFFBBBFRRR"))))
  )

(deftest parse-row-4
  (testing "row 4 index test"
    (is (= 820 (parse-line "BBFFBBFRLL"))))
  )
