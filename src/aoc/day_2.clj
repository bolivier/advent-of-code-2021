(ns aoc.day-2
  (:require [clojure.string :as str]))

(defn parse-input [file-contents]
  (map
   (fn [[direction amount*]]
     [(keyword direction) (Integer/parseInt amount*)])
   (map #(str/split % #" ")(str/split-lines file-contents))))

(defn moving? [sub]
  (seq (:course sub)))

(defprotocol ISub
  (forward [this] "Move the sub forward")
  (up [this] "Move the sub up")
  (down [this] "Move the sub down"))

(defrecord BasicSub [horizontal depth course]
  ISub
  (forward [submarine]
    (let [[_ amount] (first course)]
      (-> submarine
          (update :course rest)
          (update :horizontal + amount))))

  (up [submarine]
    (let [[_ amount] (-> submarine :course first)]
      (-> submarine
          (update :course rest)
          (update :depth - amount))))

  (down [submarine]
    (let [[_ amount] (first course)]
      (-> submarine
          (update :course rest)
          (update :depth + amount)))))

(defmulti move #(-> % :course ffirst))
(defmethod move :forward [sub] (forward sub))
(defmethod move :up [sub]  (up sub))
(defmethod move :down [sub] (down sub))

(defn create-basic-sub [course]
  (map->BasicSub {:horizontal 0
                  :depth 0
                  :course course}))

(defn get-sub-final-state [sub]
  (->> (iterate move sub)
       (drop-while moving?)
       first))

(defn solution-1 [input]
  (let [{:keys [horizontal depth]} (get-sub-final-state (create-basic-sub input))]
    (* horizontal depth)))

(defn run-1 []
  (solution-1 (parse-input (slurp "resources/day_2.input"))))

(defrecord AimSub [horizontal depth course]
  ISub
  (forward [{:keys [aim course] :as sub}]
    (let [amount (-> course first second)]
     (-> sub
         (update :course rest)
         (update :horizontal + amount)
         (assoc :depth (+ depth (* amount aim))))))
  (up [sub]
    (let [amount (-> sub :course first second)]
      (-> sub
          (update :course rest)
          (update :aim - amount))))
  (down [sub]
    (let [amount (-> sub :course first second)]
      (-> sub
          (update :course rest)
          (update :aim + amount)))))

(defn create-aim-sub [input]
  (map->AimSub {:horizontal 0
                :aim        0
                :depth      0
                :course     input}))

(defn solution-2 [input]
  (let [{:keys [horizontal depth]} (get-sub-final-state (create-aim-sub input))]
    (* horizontal depth)))

(defn run-2 []
  (solution-2 (parse-input (slurp "resources/day_2.input"))))
