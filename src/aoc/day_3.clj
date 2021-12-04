(ns aoc.day-3
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn most-common-bit
  "Takes a list of bits (type int) and returns the most common ones

  returns nil in the case of a tie."
  ([coll default]
   (let [sum (transduce (map {0 -1
                              1 1})
                        +
                        coll)]
     (cond
       (pos? sum) 1
       (neg? sum) 0
       :else default)))
  ([coll] (most-common-bit coll nil)))

(defn least-common-bit
  "Takes a list of bits (type int) and returns the most common ones

  returns nil in the case of a tie."
  ([coll default]
   (get {0 1
         1 0}
        (most-common-bit coll)
        default))
  ([coll] (least-common-bit coll nil)))

(defn bitstring->decimal [bs]
  (Long/parseLong bs 2))

(defn report-columns [diagnostic-report]
  (apply
   (partial mapv vector)
   (map vec diagnostic-report)))

(defn get-power-consumption
  "Takes a diagnostic report and returns the power consumption

  (gamma rate * epsilon rate)"
  [diagnostic-report]
  (let [by-indexes (report-columns diagnostic-report)
        gamma-rate (bitstring->decimal (str/join "" (map most-common-bit by-indexes)))
        epsilon-rate (bitstring->decimal (str/join "" (map least-common-bit by-indexes)))]
    (* epsilon-rate
       gamma-rate)))

(defn solution-1 []
  (get-power-consumption (str/split-lines (slurp "resources/day_3.input"))))


(defn nth-bit
  "Get the nth bit from "
  [i n]
  (bit-and
   1
   (bit-shift-right i n)))

(defn bin-string [n]
  (Integer/toBinaryString n))

(defn get-o2-rating [contenders bit-idx]
  (let [checking-bit
        (most-common-bit (mapv #(nth-bit % bit-idx) contenders)
                         1)]
    (if (= 1 (count contenders))
      (first contenders)
      (recur
       (filter #(= checking-bit (nth-bit % bit-idx)) contenders)
       (dec bit-idx)))))

(defn get-co2-rating [contenders bit-idx]
  (let [checking-bit
        (least-common-bit (mapv #(nth-bit % bit-idx) contenders)
                          0)]
    (if (= 1 (count contenders))
      (first contenders)
      (recur
       (filter #(= checking-bit (nth-bit % bit-idx)) contenders)
       (dec bit-idx)))))

(defn parse-binary-file-contents [file-contents]
  (map
   (fn [binary-line]
     (Long/parseLong binary-line 2))
   file-contents))

(let [file-contents (str/split-lines (slurp "resources/day_3.input"))
      max-bit (-> file-contents first count)
      diagnostic-report (parse-binary-file-contents file-contents)
      co2-rating (get-co2-rating diagnostic-report max-bit)
      o2-rating (get-o2-rating diagnostic-report max-bit)]
  (* co2-rating
     o2-rating))

(comment

  (def file-contents "
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

  (def diagnostic-report (str/split-lines (str/trim file-contents)))

  10110
  (def diagnostic-report
    (with-open [rdr (io/reader (char-array (str/trim file-contents)))]
      (read-binary-file-by-lines rdr)))

  (def diagnostic-report (read-binary-file-by-lines))
  nil)
