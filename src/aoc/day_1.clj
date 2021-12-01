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

(defn sum [coll]
  (apply + coll))

(defn solution-2 [input]
  (->> input
       (partition 3 1)
       (map sum)
       (partition 2 1)
       (filter (fn [[prev next]]
                 (< prev next)))
       count))

(comment
  (def input [199
              200
              208
              210
              200
              207
              240
              269
              260
              263])

  (def input (read-integer-file-lines "resources/day_1.input"))
  )
