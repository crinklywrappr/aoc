(ns crinklywrappr.aoc.2025.day03
  (:require [clojure.java.io :as io]))

(def file (io/resource "2025/day03.txt"))

(defn char->num [x] (- (byte x) 48))

(defn max-battery [bank remaining]
  (transduce
   (map-indexed vector)
   (completing
    (fn [a b]
      (max-key second b a))
    (fn [[max-index _]]
      (drop max-index bank)))
   [0 -1] (drop-last remaining bank)))

(defn max-joltage [len bank]
  (loop [joltage "" remaining (dec len) bank (map char->num bank)]
    (let [[digit & bank'] (max-battery bank remaining)]
      (cond
        (zero? remaining) (parse-long (str joltage digit))
        (== remaining (count bank')) (parse-long (apply str joltage digit bank'))
        :else (recur (str joltage digit) (dec remaining) bank')))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map (partial max-joltage 2))
        (transduce + (line-seq rdr)))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map (partial max-joltage 12))
        (transduce + (line-seq rdr)))))
