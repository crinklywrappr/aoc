(ns crinklywrappr.aoc.2025.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2025/day07.txt"))

(defn parse-line [line]
  (set (util/string-indices "^" line)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing
      (fn [[n beams] splitters]
        (reduce
         (fn [[n beams] beam]
           (if (contains? splitters beam)
             [(inc n) (conj beams (dec beam) (inc beam))]
             [n (conj beams beam)]))
         [n #{}] beams))
      first)
     [0 #{(sg/index-of (.readLine rdr) "S")}]
     (line-seq rdr))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing
      (fn [beams splitters]
        (reduce
         (fn [beams [beam worlds]]
           (if (contains? splitters beam)
             (-> beams
                 (update (dec beam) (fnil + 0) worlds)
                 (update (inc beam) (fnil + 0) worlds))
             (update beams beam (fnil + 0) worlds)))
         {} beams))
      #(apply + (vals %)))
     {(sg/index-of (.readLine rdr) "S") 1}
     (line-seq rdr))))
