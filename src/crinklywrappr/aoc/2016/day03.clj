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

(defn part1 []
  (letfn [(f ([a] a)
            ([a b] (if (valid-triangle? b) (inc a) a)))]
    (with-open [rdr (io/reader file)]
      (transduce (map parse) f 0 (line-seq rdr)))))

(defn parse-triplets [l1 l2 l3]
  (let [[a b c] (parse l1)
        [d e f] (parse l2)
        [g h i] (parse l3)]
    [[a d g] [b e h] [c f i]]))

(defn part2 []
  (letfn [(f [a b] (if (valid-triangle? b) (inc a) a))]
    (with-open [rdr (io/reader file)]
      (reduce f 0 (util/chunk-seq 3 parse-triplets (line-seq rdr))))))
