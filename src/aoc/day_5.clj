(ns aoc.day-5
  (:require [clojure.string :as str]
            [aoc.utils :refer [parse-int zip]]))

(def input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def vents (parse-input input))

(defn parse-point [s]
  (mapv parse-int (str/split s #",")))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(str/split %  #" -> "))
       (mapv #(mapv parse-point %))))

(defn slope-free?
  "Vents have the shape [[x1 y1] [x2 y2]]

  Slope free vents have either (= x1 x2) or (= y1 y2)"
  [vent]
  (let [[p1 p2] vent
        [x1 y1] p1
        [x2 y2] p2]
    (or (= x1 x2)
        (= y1 y2))))

(defn range2
  "Inclusive range, works in reverse without arg

  Returns infinite sequence when start == stop"
  [start stop]
  (cond
    (< start stop) (range start (inc stop))

    (< stop start) (reverse (range stop (inc start)))

    (= stop start) (repeat start)))

(defn expand-segment [vent]
  (let [[[x1 y1] [x2 y2]] vent
        xrange (range2 x1 x2)
        yrange (range2 y1 y2)]
    (zip xrange yrange)))

(defn count-intersecting-vents [vents]
  (count (reduce
          (fn [acc [k v]]
            (if (< 1 v)
              (assoc acc k v)
              acc))
          {}
          (frequencies (mapcat expand-segment vents)))))

(defn solution-1 []
  (let [vents (parse-input (slurp "resources/day_5.input"))]
    (count-intersecting-vents (filter slope-free? vents))))

(defn solution-2 []
  (let [vents (parse-input (slurp "resources/day_5.input"))]
    (count-intersecting-vents vents)))

(solution-1)
(solution-2)
