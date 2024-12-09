(ns crinklywrappr.aoc.2022.day01
  (:require [clojure.java.io :as io]))

(def file (io/resource "2022/day01.txt"))

(defn max-calories [[maximum total] calories]
  (if (nil? calories)
    [(if (> total maximum) total maximum) 0]
    [maximum (+ total calories)]))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-long)
        (transduce (completing max-calories)
                   [0 0] (line-seq rdr))
        first)))

(defn sum-calories [[totals total] calories]
  (if (nil? calories)
    [(conj totals total) 0]
    [totals (+ total calories)]))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map parse-long)
                    (completing sum-calories)
                    [[] 0])
         first (sort >) (take 3) (apply +))))
