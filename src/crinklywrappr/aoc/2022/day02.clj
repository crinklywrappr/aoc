(ns crinklywrappr.aoc.2022.day02
  (:require [clojure.java.io :as io]))

(def file (io/resource "2022/day02.txt"))

(def scores
  {"A X" (+ 1 3)
   "A Y" (+ 2 6)
   "A Z" (+ 3 0)
   "B X" (+ 1 0)
   "B Y" (+ 2 3)
   "B Z" (+ 3 6)
   "C X" (+ 1 6)
   "C Y" (+ 2 0)
   "C Z" (+ 3 3)})

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce (map scores) + 0 (line-seq rdr))))

(def target-scores
  {"A X" (+ 0 3)
   "A Y" (+ 3 1)
   "A Z" (+ 6 2)
   "B X" (+ 0 1)
   "B Y" (+ 3 2)
   "B Z" (+ 6 3)
   "C X" (+ 0 2)
   "C Y" (+ 3 3)
   "C Z" (+ 6 1)})

(defn part2 []
  (with-open [rdr (io/reader file)]
    (transduce (map target-scores) + 0 (line-seq rdr))))
