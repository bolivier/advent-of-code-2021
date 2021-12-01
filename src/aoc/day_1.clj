(ns aoc.day-1
  (:require [aoc.utils :refer [increasing? sum read-integer-file-lines]]))

(defn solution-1
  "Given your input, find the number of times the depth measurement
  increasees"
  [input]
  (->> input
       (partition 2 1)
       (filter #(apply increasing? %))
       count))

(defn solution-2
  "Use a 3 element sliding window instead of single elements to measure
  incresae/decrease."
  [input]
  (let [sliding-windows (partition 3 1 input)
        sliding-window-totals (map sum sliding-windows)]
    (solution-1 sliding-window-totals)))

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
