(ns crinklywrappr.aoc.2023.day11
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day11.txt"))

(defn index-galaxies [expansion]
  (fn index-galaxies' [[galaxies empty-columns row] line]
    (if-let [gs (seq (util/string-indices "#" line))]
      (reduce
       (fn update-state [[galaxies empty-columns next-row] col]
         [(conj galaxies [row col])
          (disj empty-columns col)
          next-row]) [galaxies empty-columns (inc row)] gs)
      [galaxies empty-columns (+ row expansion)])))

(defn update-columns [expansion]
  (fn update-columns' [galaxies col]
    (reduce
     (fn [galaxies [g-row g-col :as galaxy]]
       (if (> g-col col)
         (conj galaxies [g-row (+ g-col expansion)])
         (conj galaxies galaxy)))
     [] galaxies)))

(defn read-galaxy-map [expansion]
  (with-open [rdr (io/reader file)]
    (let [lines (line-seq rdr)
          empty-columns (into #{} (range (count (first lines))))
          [galaxies empty-columns] (reduce (index-galaxies expansion) [[] empty-columns 0] lines)]
      (reduce (update-columns (dec expansion)) galaxies (sort > empty-columns)))))

(defn solve [expansion]
  (let [galaxies (read-galaxy-map expansion)]
    (->>
     (for [[i1 [r1 c1]] (map-indexed vector galaxies)
           [i2 [r2 c2]] (map-indexed vector galaxies)
           :when (< i1 i2)]
       (+ (- r2 r1) (abs (- c2 c1))))
     (apply +))))

(defn part1 [] (solve 2))

(defn part2 [] (solve 1000000))
