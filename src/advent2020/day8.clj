(ns advent2020.day8
  (:require [clojure.string :as str]
            [util]))
(require '[clojure.core.match :refer [match]])

(def input-val (util/read-input "day8.txt"))

(defn parse-one-line [input-line]
  (let [[inst arg] (str/split input-line #" ")]
    [(keyword inst) (Integer/parseInt arg)]))

(defn parse [input-list]
  (vec (map parse-one-line input-list)))

(defn solve [instructions]
  (loop [inst-history-set #{}, value 0, inst-idx 0]
    (if (contains? inst-history-set inst-idx)
      value
      (let [[instruction argument] (get instructions inst-idx)]
          (match [instruction]
                 [:nop] (recur (conj inst-history-set inst-idx) value (inc inst-idx))
                 [:acc] (recur (conj inst-history-set inst-idx) (+ value argument) (inc inst-idx))
                 [:jmp] (recur (conj inst-history-set inst-idx) value (+ argument inst-idx)))
          ))))

(->> input-val parse solve)
