(ns aoc.day-4
  (:require [clojure.string :as str]
            [aoc.utils :refer [zip parse-int]]))

(def input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(def boards (:boards (parse-input input)))
(def draws (:draws (parse-input input)))

(def size 5)
(defn bingo-get [board [x y]]
  (get board (+ (* x 5) y)))

(defn parse-board [board]
  (into []
        (remove empty? (str/split board #"\s+"))))

(defn parse-input [input]
  (let [[draws* & boards*] (str/split input #"\n\n")
      boards (mapv parse-board boards*)
      draws (str/split draws* #",")]
    {:boards boards
     :draws draws}))

(defn mark [board element]
  (mapv #(if (= % element)
           nil
           %)
        board))

(defn winner? [board]
  (let [rows (partition 5 board)]
   (if (or (some #(every? nil? %) rows)
        (some #(every? nil? %) (apply zip rows)))
     board)))
(def loser? (complement winner?))

(defn winning-board [draws boards]
  (loop [draws draws
         boards boards
         last-picked nil]
   (if-let [winner (some winner? boards)]
     {:board winner
      :draw last-picked}
     (let [chosen-one (first draws)]
       (recur (rest draws)
              (map #(mark % chosen-one) boards)
              chosen-one)))))

(defn losing-board [draws boards]
 (loop [draws draws
        boards boards
        last-picked nil
        losing-board nil]
   (let [chosen-one (first draws)
         loser-boards (filter loser? boards)]
     (cond
       (= 1 (count loser-boards))
       (recur (rest draws)
              (map #(mark % chosen-one) boards)
              chosen-one
              (mark (first loser-boards) chosen-one))

       (= 0 (count loser-boards))
       {:board losing-board
        :draw last-picked}

       :else
       (recur (rest draws)
              (map #(mark % chosen-one) boards)
              chosen-one
              losing-board)))))

(losing-board draws boards)

(defn get-board-score [board]
  (apply + (map parse-int (remove nil? board))))

(defn solution-1 []
 (let [input (slurp "resources/day_4.input")
       {:keys [draws boards]} (parse-input input)]
   (let [{:keys [board draw]} (winning-board draws boards)]
     (* (get-board-score board)
        (parse-int draw)))))

(defn solution-2 []
  (let [input (slurp "resources/day_4.input")
        {:keys [draws boards]} (parse-input input)]
    (let [{:keys [board draw]} (losing-board draws boards)]
      (* (get-board-score board)
         (parse-int draw)))))

(mapv parse-int ["22" "13" nil nil nil
                 "8"   nil nil nil nil
                 nil   nil nil nil nil
                 "6"   nil "3" "18"nil
                 "1" "12" "20" "15""19"])
