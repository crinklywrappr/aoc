(ns crinklywrappr.aoc.2022.day03
  (:require [clojure.java.io :as io]
            [clojure.set :as st]))

(def file (io/resource "2022/day03.txt"))

(defn shared-item [line]
  (let [xs (vec line)
        c (/ (count xs) 2)]
    (first
     (st/intersection
      (set (subvec xs 0 c))
      (set (subvec xs c))))))

(defn value-item [x]
  (let [n (long x)]
    (if (> n 96)
      (- n 96)
      (- n 38))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp value-item shared-item))
        (transduce + 0 (line-seq rdr)))))

(defn select-badge [lines]
  (->> (map set lines)
       (apply st/intersection)
       first))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (comp(partition-all 3)
           (map (comp value-item select-badge)))
        (transduce + 0 (line-seq rdr)))))
