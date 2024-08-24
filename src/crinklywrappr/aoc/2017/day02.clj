(ns crinklywrappr.aoc.2017.day02
  (:require [clojure.java.io :as io]))

(def file (io/resource "2017/day02.txt"))

(defn parse [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn greatest-diff [nums]
  (- (apply max nums) (apply min nums)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp greatest-diff parse))
        (transduce + (line-seq rdr)))))

(defn evenly-divisible [nums]
  (->>
   (for [x nums y nums
         :when (zero? (rem x y))]
     (/ x y))
   (apply max)))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp evenly-divisible parse))
        (transduce + (line-seq rdr)))))
