(ns crinklywrappr.aoc.2018.day02
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]
            [clojure.set :as st]))

(def file (io/resource "2018/day02.txt"))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map (comp st/map-invert frequencies))
     (completing
      (fn [agg m]
        (cond-> agg
          (contains? m 2) (update 2 inc)
          (contains? m 3) (update 3 inc)))
      (fn [agg] (* (get agg 2) (get agg 3))))
     {2 0 3 0} (line-seq rdr))))

(defn match? [c1 c2]
  (when (= c1 c2) c1))

(defn matching-chars [s1 s2]
  (apply str (filter some? (map match? s1 s2))))

(defn part2 []
  (let [lines (with-open [rdr (io/reader file)] (doall (line-seq rdr)))]
    (first
     (for [s1 lines
           s2 (rest lines)
           :when (== (util/hamming-distance s1 s2) 1)]
       (matching-chars s1 s2)))))
