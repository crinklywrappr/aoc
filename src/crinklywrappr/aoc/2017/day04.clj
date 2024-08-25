(ns crinklywrappr.aoc.2017.day04
  (:require [clojure.java.io :as io]))

(def file (io/resource "2017/day04.txt"))

(defn valid? [passphrase]
  (reduce
   (fn [a b]
     (if (contains? a b)
       (reduced nil)
       (conj a b)))
   #{(first passphrase)}
   (rest passphrase)))

(defn part1 []
  (letfn [(parse [line] (re-seq #"[a-z]+" line))]
    (with-open [rdr (io/reader file)]
      (->> (line-seq rdr)
           (keep (comp valid? parse))
           count))))

(defn part2 []
  (letfn [(parse [line]
            (map frequencies (re-seq #"[a-z]+" line)))]
    (with-open [rdr (io/reader file)]
      (->> (line-seq rdr)
           (keep (comp valid? parse))
           count))))
