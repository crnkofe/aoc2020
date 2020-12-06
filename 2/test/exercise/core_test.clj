(ns exercise.core-test
  (:require [clojure.test :refer :all]
            [exercise.core :refer :all]))

; asdasd {:char \c :min 10 :max 100}

(deftest should-parse
  (testing "line parses nicely"
    (let [result (parse-line "10-100 c: test")]
      (is (= result  [{:min 10 :max 100 :ch \c } {\t 2 \e 1 \s 1}] ))
      )
    )
  )

(deftest should-not-parse
  (testing "line is not parseable"
    (let [result (parse-line "")]
      (is (= result  [nil {}] ))
      )
    )
  )

(deftest should-validate-pass
  (testing "password is valid"
    (let [input "5-6 n: nnsnnznfjnf"]
      (is (= true (is-valid input)))
      )
    )
  )

(deftest should-invalidate-pass
  (testing "password is valid"
    (let [input "8-13 s: sjssqssrgssrz"]
      (is (= false (is-valid input)))
      )
    )
  )

(deftest should-parse-positions
  (testing "line parses nicely"
    (let [result (parse-positions "10-100 c")]
      (is (= result {:first 9 :second 99 :ch \c } ))
      )
    )
  )

(deftest should-validate-pass-part2
  (testing "password is valid-part2"
    (let [input "5-6 n: nnsnnznfjnf"]
      (is (= true (is-valid-part2 input)))
      )
    )
  )


(deftest should-validate-pass-part2
  (testing "password is valid-part2"
    (let [input "5-6 n: nnsnoznfjnf"]
      (is (= false (is-valid-part2 input)))
      )
    )
  )

(deftest should-parse-part2
  (testing "line parses nicely"
    (let [result (parse-line-part2 "10-100 c: test")]
      (is (= result  [{:first 9 :second 99 :ch \c } "test"] ))
      )
    )
  )
