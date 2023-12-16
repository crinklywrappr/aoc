(ns crinklywrappr.aoc.2023.day14
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day14.txt"))

(defn tilt' [[row round blockers] [col boulder]]
  (if (= boulder "#")
    [row round (assoc blockers col row)]
    (let [stop (inc (get blockers col -1))]
      [row
       (if (contains? round stop)
         (update round stop inc)
         (assoc round stop 1))
       (assoc blockers col stop)])))

(defn tilt [[row round blockers] line]
  (let [objects (util/re-pos #"#|O" line)]
    (reduce tilt' [(inc row) round blockers] objects)))

(defn calc-load [row]
  (fn calc-load' [total [r boulders]]
    (+ total (* (- row r) boulders))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (let [[row round blockers] (reduce tilt [-1 {} {}] (line-seq rdr))]
      (reduce (calc-load (inc row)) 0 round))))
