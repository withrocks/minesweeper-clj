(ns minesweeper.core-test
  (:require [clojure.test :refer :all]
            [minesweeper.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest a-test
  (testing "FIXME, I fail."
    (is (place-bombs (create-board 3 3) 3 3 3))))
    
