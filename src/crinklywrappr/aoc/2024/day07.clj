(ns crinklywrappr.aoc.2024.day07
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]
            [clojure.math.combinatorics :as combo]))

(def file (io/resource "2024/day07.txt"))

(defn parse-line [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn apply-operator [solution total [operator input]]
  (let [total' (operator total input)]
    (if (> total' solution)
      (reduced total')
      total')))

(defn solve [solution total equation]
  (let [solution' (reduce (partial apply-operator solution)
                          (first equation)
                          (partition 2 (rest equation)))]
    (if (== solution' solution)
      (reduced (+ total solution))
      total)))

(defn valid-inputs [operators total [solution & inputs]]
  (let [operations (combo/selections operators (dec (count inputs)))]
    (->> (map util/interleave-all (repeat (count operations) inputs) operations)
         (reduce (partial solve solution) total))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce (completing (partial valid-inputs [+ *]))
                   0 (line-seq rdr)))))

(defn concat-numbers [a b]
  (parse-long (str a b)))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce (completing (partial valid-inputs [+ * concat-numbers]))
                   0 (line-seq rdr)))))
