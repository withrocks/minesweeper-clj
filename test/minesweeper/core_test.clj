(ns minesweeper.core-test
  (:require [clojure.test :refer :all]
            [minesweeper.core :refer :all]))

(deftest test-board-to-string
  (testing "A newly initialized board is shown correctly"
    (let [board (create-board 2 3)]
      (is (= (board-to-string board true)
             "-0 |-0 |-0 \n-0 |-0 |-0 "))
      (is (= (board-to-string board false)
             "x|x|x\nx|x|x")))))

(deftest test-board-with-bombs-to-string
  (testing "A board with bombs is shown correctly"
    (let [board (place-bombs
                 (create-board 2 3) [0 2 5])]
      (is (= (board-to-string board true)
             "b0 |-0 |b0 \n-0 |-0 |b0 "))
      (is (= (board-to-string board false)
             "x|x|x\nx|x|x")))))

; In the following tests, we have the following board:
; b0 |-0 |-0 
; -0 |-0 |-0  
; If the user steps on [0,0], he dies
; If the user steps on any of those surrounding [0,0], that field should change to '-1s'
; where s stands for "stepped on". Other fields should not change.
; If the user steps on [0,2] or [1,2], they should change to '-0s' and they should recursively
; affect surrounding squares, until a square that has a bomb next to it is reached.
(deftest test-stepped-on-dies
  (testing "A stepped on board with bombs is shown correctly"
    (let [board (create-test-board)
          board (make-move board 0)]
      (println board-to-string board true)
      (is (= (board-to-string board true)
             "b0s|-0 |-0 \n-0 |-0 |-0 "))
      (is (= (board-to-string board false)
             "b|x|x\nx|x|x"))
      (is (:dead board)))))

; Selecting [1,2] in this case should lead to: 
; x|1|#
; x|1|#
(deftest test-stepped-on-spreads
  (testing "A stepped on board with bombs is shown correctly"
    (let [board (create-board 2 3)
          board (place-bombs board [0])
          board (make-move board 5)]
      (is (= (board-to-string board true)
             "-2 |b2 |-2 |-1 \nb2 |-0 |-0s"))
      (is (= (board-to-string board false)
             "x|1|#\nx|1|#")))))
