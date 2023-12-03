(ns crinklywrappr.aoc.2023.day3
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day3.txt"))

(def syms #"[^\d.]")
(def nums #"\d+")

(defn symbols-adjacent? [symbols [num-idx number]]
  (some symbols (range (dec num-idx) (+ num-idx (count number) 1))))

(defn part-numbers [[sum above line] below]
  (let [symbols (->> (merge (util/re-pos syms above)
                            (util/re-pos syms line)
                            (util/re-pos syms below))
                     keys set)
        numbers (util/re-pos nums line)]
    [(-> (comp (filter (partial symbols-adjacent? symbols))
            (map (comp parse-long second)))
         (transduce + numbers) (+ sum))
     line below]))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (let [wrap (apply str (repeat 140 \.))
          lines (util/wrap-line-seq rdr wrap)]
      (first (reduce part-numbers [0 wrap (first lines)] (rest lines))))))

(defn number-adjacent? [symbol [[minimum maximum] _]]
  (or (<= minimum (dec symbol) maximum)
      (== symbol minimum)
      (<= minimum (inc symbol) maximum)))

(defn gear-ratio [numbers symbol]
  (let [adjacent (->> numbers
                      (filterv (partial number-adjacent? symbol))
                      (mapcat second))]
    (if (== (count adjacent) 2)
      (apply * (mapv parse-long adjacent)) 0)))

(defn gather-numbers [line]
  (reduce
   (fn [a [idx number]]
     (assoc a [idx (+ idx (dec (count number)))] [number]))
   {} (util/re-pos nums line)))

(defn gear-ratios [[sum above line] below]
  (let [numbers (merge-with concat
                            (gather-numbers above)
                            (gather-numbers line)
                            (gather-numbers below))
        symbols (keys (util/re-pos #"\*" line))]
    [(-> (map (partial gear-ratio numbers))
         (transduce + symbols) (+ sum))
     line below]))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (let [wrap (apply str (repeat 140 \.))
          lines (util/wrap-line-seq rdr wrap)]
      (first (reduce gear-ratios [0 wrap (first lines)] (rest lines))))))
