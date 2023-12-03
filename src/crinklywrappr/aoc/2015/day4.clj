(ns crinklywrappr.aoc.2015.day4
  (:require [clj-commons.digest :as digest]
            [clojure.string :as sg]))

(defn md5-starts-with? [expectation prefix suffix]
  (when (-> prefix (str suffix) digest/md5
            (sg/starts-with? expectation))
    suffix))

(defn part1 [] (some (partial md5-starts-with? "00000" "ckczppom") (range)))

(defn part2 [] (some (partial md5-starts-with? "000000" "ckczppom") (range)))
