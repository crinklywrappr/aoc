(ns crinklywrappr.aoc.2017.day01
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util])
  (:import [java.io BufferedReader Closeable]))

(def file (io/resource "2017/day01.txt"))

(def digits (butlast (slurp file)))

(defn checksum [a [x y]]
  (if (= x y)
    (+ a (- (byte x) 48))
    a))

(defn part1 []
  (->> (map vector digits (rest (cycle digits)))
       (reduce checksum 0)))

(defn part2 []
  (let [offset (/ (count digits) 2)]
    (->> (map vector digits (drop offset (cycle digits)))
         (reduce checksum 0))))
