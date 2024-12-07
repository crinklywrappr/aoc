(ns crinklywrappr.aoc.2024.day07
  (:require [clojure.math :as math]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def file (io/resource "2024/day07.txt"))

(defn parse-line [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn solve-equation [solution inputs operations]
  (loop [solution' (first inputs) [x & xs] (rest inputs) [op & ops] operations]
    (if (or (nil? x) (> solution' solution))
      solution'
      (recur (op solution' x) xs ops))))

(defn solvable? [solution inputs total operation]
  (if (== solution (solve-equation solution inputs operation))
    (reduced (+ total solution))
    total))

(defn valid-inputs [operators total [solution & inputs]]
  (->> (combo/selections operators (dec (count inputs)))
       (reduce (partial solvable? solution inputs) total)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce (completing (partial valid-inputs [+ *]))
                   0 (line-seq rdr)))))

(defn concat-numbers [a b]
  (+ b (* a (long (math/pow 10 (inc (math/floor (math/log10 b))))))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce (completing (partial valid-inputs [+ * concat-numbers]))
                   0 (line-seq rdr)))))
