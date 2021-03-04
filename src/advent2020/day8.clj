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


(defn get-next-state [state]
  (let [{:keys [inst-history-set value inst-idx instructions]} state
        [instruction argument] (get instructions inst-idx)]
    (match [instruction]
           [:nop] {:inst-history-set (conj inst-history-set inst-idx)
                   :value value
                   :inst-idx (inc inst-idx)
                   :instructions instructions}
           [:acc] {:inst-history-set (conj inst-history-set inst-idx)
                   :value (+ value argument)
                   :inst-idx (inc inst-idx)
                   :instructions instructions}
           [:jmp] {:inst-history-set (conj inst-history-set inst-idx)
                   :value value
                   :inst-idx (+ argument inst-idx)
                   :instructions instructions})
    ))

(defn continue? [{:keys [inst-history-set inst-idx]}]
  (not (contains? inst-history-set inst-idx)))

(defn get-initial-state [instructions]
  {:inst-history-set #{}
   :value 0
   :inst-idx 0
   :instructions instructions})

(get (->> input-val
     (parse)
     (get-initial-state)
     (iterate get-next-state)
     (drop-while continue?)
     first)
     :value)
