(ns crinklywrappr.aoc.2016.day03
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2016/day03.txt"))

(defn parse [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn valid-triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(defn acc [a b]
  (if (valid-triangle? b) (inc a) a))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce (map parse) (completing acc) 0 (line-seq rdr))))

(defn parse-triplets [[t1 t2 t3]]
  (let [[a b c] t1
        [d e f] t2
        [g h i] t3]
    [[a d g] [b e h] [c f i]]))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (transduce
     (comp
      (map parse)
      (partition-all 3)
      (mapcat parse-triplets))
     (completing acc) 0 (line-seq rdr))))
