(ns aoc.day-6
  (:require [aoc.utils :refer [map-keys]]))

(defn parse [file-contents]
  (read-string (str "[" file-contents "]")))

(defn tick [coll]
  (let [coll' (mapv (fn [n]
                      (if (zero? n)
                        6
                        (dec n)))
                    coll)
        fish-to-add (->> coll
                         (filter zero?)
                         count)
        new-fish (->> fish-to-add
                      range
                      (map (constantly 8))
                      (apply vector))]
    (into []
          (concat coll' new-fish))))

(defn count-after-n-days [input n]
 (count (nth (iterate tick input)
             n)))

(defn solution-1 []
  (let [input (parse (slurp "resources/day_6.input"))]
    (count-after-n-days input 80)))
;; 345387

(defn solution-2-naive
  "This is too slow.

  I am going to look for a sparser representation."
  []
  (let [input (parse (slurp "resources/day_6.input"))]
    (count-after-n-days input 256)))

(defn tick' [coll]
  (let [coll' (map-keys dec coll)
        reproducing-fish (get coll' -1 0)]
    (-> coll'
        (dissoc -1)
        (update 6 (fn [six]
                    (+ reproducing-fish (or six 0))))
        (update 8 (fn [eight]
                    (+ reproducing-fish (or eight 0)))))))

(defn count-days [freqs days]
  (let [fish-seq (iterate tick' freqs)
        day-of-fish (nth fish-seq days)]
    (->> day-of-fish
         vals
         (apply +))))

(defn solution-2 []
  (let [input (slurp "resources/day_6.input")
        freqs (frequencies (parse input))]
    (count-days freqs 256)))
;; 1574445493136


(comment
  (def file-contents "3,4,3,1,2")
  (def input (read-string (str "[" file-contents "]")))
nil)
