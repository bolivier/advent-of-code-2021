(ns aoc.utils
  (:require [clojure.java.io :as io]))

(defn increasing? [prev next]
  (< prev next))

(defn sum [coll]
  (apply + coll))

(defn read-integer-file-lines [path]
  (->> path
       io/reader
       line-seq
       (map #(Integer/parseInt %))))
