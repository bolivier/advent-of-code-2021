(ns aoc.day-1
  (:require [clojure.java.io :as io]))

(defn read-integer-file-lines [path]
  (->> "resources/day_1.input"
       io/reader
       line-seq
       (map #(Integer/parseInt %))))

(defn solution-1
  "Given your input, find the number of times the depth measurement
  increasees"
  []
  (->> "resources/day_1.input"
       read-integer-file-lines
       (partition 2 1)
       (filter (fn [[prev next]]
                 (< prev next)))
       count))
