(ns crinklywrappr.aoc.2024.day10
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]))

(def file (io/resource "2024/day10.txt"))

(defn parse-line [line]
  (mapv #(- (long %) 48) line))

(def gsource
  (with-open [rdr (io/reader file)]
    (reduce
     (fn [a b] (conj a (parse-line b)))
     [] (line-seq rdr))))

(defn elevation-at [row col]
  (get-in gsource [row col]))

(defn top? [[_ _ e]] (== e 9))

(defn trailhead?
  ([[_ _ e]] (== e 0))
  ([row col] (zero? (elevation-at row col))))

(defn graph [row col]
  (z/zipper
   (fn branch? [node] (not (top? node)))
   (fn children [[row col elevation]]
     (->>
      [[(dec row) col (elevation-at (dec row) col)]
       [row (inc col) (elevation-at row (inc col))]
       [(inc row) col (elevation-at (inc row) col)]
       [row (dec col) (elevation-at row (dec col))]]
      (keep
       (fn [[r c e :as node]]
         (when (and (some? e) (== (- e elevation) 1))
           node)))
      seq))
   (constantly nil) ;; don't need this
   [row col (elevation-at row col)]))

(defn score-trail [g]
  (loop [tops #{} g g]
    (let [node (z/node g)]
      (cond
        (z/end? g) (count tops)
        (top? node) (recur (conj tops node) (z/next g))
        :else (recur tops (z/next g))))))

(defn part1 []
  (->>
   (for [row (range (count gsource))
         col (range (count gsource))
         :when (trailhead? row col)
         :let [g (graph row col)]]
     (score-trail g))
   (apply +)))

(defn rate-trail [g]
  (loop [rating 0 g g]
    (cond
      (z/end? g) rating
      (top? (z/node g)) (recur (inc rating) (z/next g))
      :else (recur rating (z/next g)))))

(defn part2 []
  (->>
   (for [row (range (count gsource))
         col (range (count gsource))
         :when (trailhead? row col)
         :let [g (graph row col)]]
     (rate-trail g))
   (apply +)))
