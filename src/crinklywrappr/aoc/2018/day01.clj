(ns crinklywrappr.aoc.2018.day01
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2018/day01.txt"))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce (map parse-long) + (line-seq rdr))))

(defn part2 []
  (with-open [rdr (util/cyclic-reader file)]
    (transduce
     (map parse-long)
     (completing
      (fn [[freq seen] change]
        (let [new-freq (+ freq change)]
          (if (seen new-freq)
            (reduced new-freq)
            [new-freq (conj seen new-freq)]))))
     [0 #{0}] (line-seq rdr))))
