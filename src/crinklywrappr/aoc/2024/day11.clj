(ns crinklywrappr.aoc.2024.day11
  (:require [clojure.java.io :as io]
            [clojure.math :as math]))

(def file (io/resource "2024/day11.txt"))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(def stones
  (reduce (fn [a b] (update a b (fnil inc 0)))
          {} (parse-line (slurp file))))

(defn magnitude [x]
  (inc (math/floor (math/log10 x))))

(defn split-number [x mag]
  (let [k (math/pow 10 (/ mag 2))
        front (long (math/floor (/ x k)))]
    [front (long (- x (* front k)))]))

(defn apply-rule [x]
  (let [mag (magnitude x)]
    (cond
      (zero? x) 1
      (zero? (mod mag 2)) (split-number x mag)
      :else (* x 2024))))

(defn blink [xs]
  (reduce-kv
   (fn [a k v]
     (let [x (apply-rule k)]
       (if (number? x)
         (update a x (fnil + 0) v)
         (-> a
             (update (first x) (fnil + 0) v)
             (update (second x) (fnil + 0) v)))))
   {} xs))

(defn part1 []
  (loop [stones stones i 0]
    (if (== i 25)
      (apply + (vals stones))
      (recur (blink stones) (inc i)))))

(defn part2 []
  (loop [stones stones i 0]
    (if (== i 75)
      (apply + (vals stones))
      (recur (blink stones) (inc i)))))
