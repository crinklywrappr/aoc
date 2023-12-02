(ns crinklywrappr.aoc.2015.day1
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2015/day1.txt"))

(defn change-floor [floor character]
  (cond
    (= character \() (inc floor)
    (= character \)) (dec floor)
    :else floor))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (reduce change-floor 0 (util/char-seq rdr))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (->> (util/char-seq rdr)
         (util/reduce-while
          (fn [[index floor]] (not= floor -1))
          (fn [[index floor] character]
            [(inc index) (change-floor floor character)])
          [0 0])
         first inc)))
