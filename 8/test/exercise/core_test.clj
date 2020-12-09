(ns exercise.core-test
  (:require [clojure.test :refer :all]
            [exercise.core :refer :all]))

(deftest parse-nop-test
  (testing "Parses NOP."
    (is (= {:code "NOP" :arg1 -1} (parse-instruction "NOP -1")))))

(deftest parse-jmp-test
  (testing "Parses JMP."
    (is (= {:code "JMP" :arg1 10} (parse-instruction "JMP 10")))))

(deftest parse-acc-test
  (testing "Parses ACC."
    (is (= {:code "ACC" :arg1 0} (parse-instruction "ACC +0")))))

(deftest initialize-computer-test
  (testing "Parses AC."
    (is (= {:ip 0 :accumulator 0 :code [{:code "ACC" :arg1 0}] :visited #{} :executions 0 :limit 10000} (initialize-computer [(parse-instruction "ACC +0")])))))
