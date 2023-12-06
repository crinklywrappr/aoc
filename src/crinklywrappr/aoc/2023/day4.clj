(ns crinklywrappr.aoc.2023.day4
  (:require [clojure.set :as st]
            [clojure.java.io :as io]))

(def file (io/resource "2023/day4.txt"))
(def winning 10) ;; switch to 5 for example input

(defn parse-line [line]
  (->> (re-seq #"\d+" line)
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

;; i got lucky that my puzzle input didn't
;; add fake cards past the end of the file
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
