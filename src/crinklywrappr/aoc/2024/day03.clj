(ns crinklywrappr.aoc.2024.day03
  (:require [clojure.java.io :as io]))

(def file (io/resource "2024/day03.txt"))

(defn parse-line-p1 [line]
  (->> line
       (re-seq #"mul\((\d+),(\d+)\)")
       (mapv (comp (fn [[_ a b]]
                  [(parse-long a)
                   (parse-long b)])))))

(defn sum-products-p1 [total [a b]]
  (+ total (* a b)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (mapcat parse-line-p1)
        (transduce
         (completing sum-products-p1)
         0 (line-seq rdr)))))

(defn parse-line-p2 [line]
  (re-seq #"mul\((\d+),(\d+)\)|don't\(\)|do\(\)" line))

(defn sum-products-p2
  [[total multiplier] [command a b]]
  (case command
    "do()" [total 1]
    "don't()" [total 0]
    [(+ total (* multiplier (parse-long a) (parse-long b))) multiplier]))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (mapcat parse-line-p2)
        (transduce
         (completing sum-products-p2)
         [0 1] (line-seq rdr))
        first)))
