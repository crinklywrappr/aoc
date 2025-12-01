(ns crinklywrappr.aoc.2025.day01
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2025/day01.txt"))
(def dial-size 100)

(defn parse-line [line]
  (let [[dir n] (util/split-string 1 line)]
    (case dir
      "R" (parse-long n)
      "L" (- (parse-long n)))))

(defn zero-landings [[zeros pos] n]
  (let [new-pos (mod (+ pos n) dial-size)]
    (if (zero? new-pos)
      [(inc zeros) new-pos]
      [zeros new-pos])))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing zero-landings first)
     [0 50] (line-seq rdr))))

(defn zero-clicks [[zeros pos] n]
  (let [raw-pos (+ pos n)
        new-pos (mod raw-pos dial-size)
        rotations (abs (quot raw-pos dial-size))
        crossings (cond
                    (or (zero? pos) (> raw-pos dial-size)) rotations
                    (neg? raw-pos) (inc rotations)
                    (zero? new-pos) 1
                    :else 0)]
    [(+ zeros crossings) new-pos]))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing zero-clicks first)
     [0 50] (line-seq rdr))))
