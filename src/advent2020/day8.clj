(ns advent2020.day8
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [util]))

(def input-val (util/read-input "day8.txt"))

(defn parse-one-line [input-line]
  (let [[inst arg] (str/split input-line #" ")]
    [(keyword inst) (Integer/parseInt arg)]))

(defn parse [input-list]
  (vec (map parse-one-line input-list)))

(defn continue? [{:keys [inst-history-set inst-idx]}]
  (not (contains? inst-history-set inst-idx)))

(defn make-next-state-fn [instructions]
  (fn [state]
    (let [{:keys [inst-history-set value inst-idx]} state
          [instruction argument] (get instructions inst-idx)
          state' {:inst-history-set (conj inst-history-set inst-idx)}]
      (match [instruction]
             [:nop] (assoc state', :value value, :inst-idx (inc inst-idx))
             [:acc] (assoc state', :value (+ value argument), :inst-idx (inc inst-idx))
             [:jmp] (assoc state', :value value, :inst-idx (+ argument inst-idx))))))

(def initial-state
  {:inst-history-set #{}
   :value 0
   :inst-idx 0})

(def next-state-fn
  (->> input-val parse make-next-state-fn))

(->> initial-state
     (iterate next-state-fn)
     (drop-while continue?)
     first
     :value)
