(ns crinklywrappr.aoc.2023.day4
  (:require [clojure.set :as st]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day4.txt"))
(def winning 10) ;; switch to 5 for the example input

(def rgx #"\d+")

(defn parse-line [line]
  (->> (re-seq rgx line)
       ((fn to-sets [[& nums]]
          [(parse-long (first nums))
           (set (rest (take (inc winning) nums)))
           (set (drop (inc winning) nums))]))))

(defn part1 []
  (letfn [(score-card [[card winning scratched]]
            (if-let [x (seq (st/intersection winning scratched))]
              (->> x count dec (max 0) (bit-shift-left 1)) 0))]
    (with-open [rdr (io/reader file)]
      (-> (map (comp score-card parse-line))
          (transduce + (line-seq rdr))))))

(defn part2 []
  (letfn [(score-card [[card winning scratched]]
            [card (count (st/intersection winning scratched))])
          (scratchcards [[last-card copies] [card score]]
            [card (merge-with + copies {card 1}
                              (->> (range (inc card) (+ (inc card) score))
                                   (reduce (fn [acc num]
                                             (->> (get copies card 0)
                                                  inc (assoc acc num)))
                                           {})))])]
    (with-open [rdr (io/reader file)]
      (->> (line-seq rdr)
           (mapv (comp score-card parse-line))
           (reduce scratchcards [0 {}])
           second vals (apply +)))))
