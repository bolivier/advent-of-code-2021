(ns aoc.day-2
  (:require [clojure.string :as str]))

(def file-contents "forward 5
down 5
forward 8
up 3
down 8
forward 2
")

(def input '([:forward 5] [:down 5] [:forward 8] [:up 3] [:down 8] [:forward 2]))

(defn parse-input [file-contents]
  (map
   (fn [[direction amount*]]
     [(keyword direction) (Integer/parseInt amount*)])
   (map #(str/split % #" ")(str/split-lines file-contents))))

(defn tick [submarine]
  (let [{:keys [horizontal depth course]} submarine
        [direction amount] (first course)]
    {:course (rest course)
     :horizontal (if (= :forward direction)
                   (+ amount horizontal)
                   horizontal)
     :depth (case direction
              :down (+ depth amount)
              :up (- depth amount)
              depth)}))

(defn solution-1 [input]
  (let [submarine {:horizontal 0
                   :depth 0
                   :course input}
        dest-submarine (first (drop-while
                               (fn [sub]
                                 (seq (:course sub)))
                               (iterate tick submarine)))]
    (* (:horizontal dest-submarine)
       (:depth dest-submarine))))

(defn run-1 []
  (solution-1 (parse-input (slurp "resources/day_2.input"))))

(defn tick' [{:keys [horizontal aim depth course]}]
  (let [[direction amount] (first course)]
    {:course (rest course)
     :aim (case direction
            :up (- aim amount)
            :down (+ aim amount)
            aim)
     :horizontal (if (= direction :forward)
                   (+ horizontal amount)
                   horizontal)
     :depth (if (= direction :forward)
              (+ depth (* amount aim))
              depth)}))

(defn solution-2 [input]
  (let [submarine      {:horizontal 0
                        :aim        0
                        :depth      0
                        :course     input}
        dest-submarine (first (drop-while
                               (fn [sub]
                                 (seq (:course sub)))
                               (iterate tick' submarine)))]
    (* (:horizontal dest-submarine)
       (:depth dest-submarine))))

(defn run-2 []
  (solution-2 (parse-input (slurp "resources/day_2.input"))))
