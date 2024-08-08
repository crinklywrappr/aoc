(ns crinklywrappr.aoc.2016.day02
  (:require [clojure.string :as sg]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2016/day02.txt"))

(defn digit [[digits current-digit] move]
  (if (= move \newline)
    [(+ (* digits 10) current-digit) current-digit]
    [digits
     (cond
       (and (= move \U) (contains? #{1 2 3} current-digit)) current-digit
       (and (= move \L) (contains? #{1 4 7} current-digit)) current-digit
       (and (= move \D) (contains? #{7 8 9} current-digit)) current-digit
       (and (= move \R) (contains? #{3 6 9} current-digit)) current-digit
       (= move \U) (- current-digit 3)
       (= move \L) (dec current-digit)
       (= move \D) (+ current-digit 3)
       (= move \R) (inc current-digit))]))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (first (reduce digit [0 5] (util/char-seq rdr)))))

(defn keypad-digit [[digits current-digit] move]
  (if (= move \newline)
    [(+ (* digits 16) current-digit) current-digit]
    [digits
     (cond
       (and (= move \U) (contains? #{5 2 1 4 9} current-digit)) current-digit
       (and (= move \L) (contains? #{1 2 5 0xA 0xD} current-digit)) current-digit
       (and (= move \D) (contains? #{5 0xA 0xD 0xC 9} current-digit)) current-digit
       (and (= move \R) (contains? #{1 4 9 0xC 0xD} current-digit)) current-digit
       (= move \L) (dec current-digit)
       (= move \R) (inc current-digit)
       (and (= move \U) (== current-digit 3)) 1
       (and (= move \U) (== current-digit 0xD)) 0xB
       (= move \U) (- current-digit 4)
       (and (= move \D) (== current-digit 1)) 3
       (and (= move \D) (== current-digit 0xB)) 0xD
       (= move \D) (+ current-digit 4))]))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (sg/upper-case
     (Long/toHexString
      (first (reduce keypad-digit [0 5] (util/char-seq rdr)))))))
