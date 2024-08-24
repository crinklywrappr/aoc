(ns crinklywrappr.aoc.2015.day01
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2015/day01.txt"))

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
    (reduce
     (fn [[index floor] character]
       (let [new-floor (change-floor floor character)]
         (if (== new-floor -1)
           (reduced (inc index))
           [(inc index) new-floor])))
     [0 0] (util/char-seq rdr))))
