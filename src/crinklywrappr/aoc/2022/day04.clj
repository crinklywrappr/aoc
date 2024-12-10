(ns crinklywrappr.aoc.2022.day04
  (:require [clojure.java.io :as io]))

(def file (io/resource "2022/day04.txt"))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn fully-contains? [[a b c d]]
  (or (and (<= a c) (>= b d))
      (and (<= c a) (>= d b))))

(defn count-fully-contains [total sectors]
  (if (fully-contains? sectors)
    (inc total)
    total))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce (completing count-fully-contains)
                   0 (line-seq rdr)))))

(defn overlaps? [[a b c d]]
  (or (<= a c b) (<= c a d)))

(defn count-overlaps [total sectors]
  (if (overlaps? sectors)
    (inc total)
    total))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce (completing count-overlaps)
                   0 (line-seq rdr)))))
