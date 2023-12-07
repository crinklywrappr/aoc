(ns crinklywrappr.aoc.2023.day5
  (:require [clojure.java.io :as io]))

(defn f [d x] (* x (- d x)))
(defn mid [left right] (+ left (long (/ (- right left) 2))))

(defn solve-left [d record left right]
  (loop [l left r right]
    (let [m (mid l r)]
      (cond
        (== m l) r
        (< (f d m) record) (recur m r)
        (> (f d m) record) (recur l m)
        (== (f d m) record) (inc m)))))

(defn solve-right [d record left right]
  (loop [l left r right]
    (let [m (mid l r)]
      (cond
        (== m l) l
        (< (f d m) record) (recur l m)
        (> (f d m) record) (recur m r)
        (== (f d m) record) (dec m)))))

;; all of the record times can be beaten
(defn solve-race [[d record]]
  (let [m (mid 0 d)]
    [(solve-left d record 0 m)
     (solve-right d record m d)]))

(defn part1 []
  (->> "2023/day6.txt"
       io/resource slurp
       (re-seq #"\d+")
       (map parse-long)
       (partition 4)
       (apply interleave)
       (partition 2)
       (mapv (comp inc (partial apply -)
                reverse solve-race))
       (apply *)))

(defn part2 []
  (->> "2023/day6.txt"
       io/resource slurp
       (re-seq #"\d+")
       (partition 4)
       (map (comp parse-long
                  (partial apply str)))
       solve-race
       reverse
       (apply -)
       inc))
