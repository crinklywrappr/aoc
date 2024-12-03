(ns crinklywrappr.aoc.2024.day02
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2024/day02.txt"))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn valid-asc-level? [[b i x] y]
  (if (and (< x y) (< (- y x) 4))
    [true (inc i) y]
    (reduced [false i x])))

(defn valid-desc-level? [[b i x] y]
  (if (and (> x y) (< (- x y) 4))
    [true (inc i) y]
    (reduced [false i x])))

(defn choose-validator [[x y & _]]
  (if (> x y)
    valid-desc-level?
    valid-asc-level?))

(defn first-fault [xs]
  (reduce (choose-validator xs)
          [true 0 (first xs)]
          (rest xs)))

(defn count-valid-reports [n report]
  (if (first (first-fault report))
    (inc n)
    n))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce
         (completing count-valid-reports)
         0 (line-seq rdr)))))

(defn acceptable-report? [xs]
  (let [[valid? idx] (first-fault xs)]
    (cond
      valid? true

      (and (not valid?) (zero? idx))
      (or (first (first-fault (rest xs)))
          (first (first-fault (util/pluck xs (inc idx)))))

      :else
      (or (first (first-fault (util/pluck xs (dec idx))))
          (first (first-fault (util/pluck xs idx)))
          (first (first-fault (util/pluck xs (inc idx))))))))

(defn count-acceptable-reports [n report]
  (if (acceptable-report? report)
    (inc n)
    n))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce
         (completing count-acceptable-reports)
         0 (line-seq rdr)))))
