(ns crinklywrappr.aoc.2022.day06
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2022/day06.txt"))

(defn find-marker [n]
  (with-open [rdr (io/reader file)]
    (let [xs (util/char-seq rdr)]
      (reduce
       (fn [[marker i] c]
         (if (== (count (distinct marker)) n)
           (reduced i)
           [(conj (subvec marker 1) c) (inc i)]))
       [(vec (take n xs)) n] (drop n xs)))))

(defn part1 [] (find-marker 4))

(defn part2 [] (find-marker 14))
