(ns crinklywrappr.aoc.2024.day01
  (:require [clojure.java.io :as io]))

(def file (io/resource "2024/day01.txt"))

(defn parse-line [line]
  (let [[l r] (re-seq #"\d+" line)]
    [(parse-long l) (parse-long r)]))

(defn build-lists [[ls rs] [l r]]
  [(conj ls l) (conj rs r)])

(defn sort-lists [[ls rs]]
  [(sort ls) (sort rs)])

(defn sum-lists [[ls rs]]
  (loop [total 0 [l & ls'] ls [r & rs'] rs]
    (if (nil? l)
      total
      (recur (+ total (abs (- r l))) ls' rs'))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map parse-line)
                    (completing build-lists)
                    [(list) (list)])
         sort-lists
         sum-lists)))

(defn add [[l r] l' r']
  [(+ l l') (+ r r')])

(defn build-map [m [l r]]
  (-> m
      (update l (fnil add [0 0]) 1 0)
      (update r (fnil add [0 0]) 0 1)))

(defn sum-map [total [n [l r]]]
  (+ total (* n l r)))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map parse-line)
                    (completing build-map)
                    {})
         (reduce sum-map 0))))

