(ns crinklywrappr.aoc.2024.day02
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2024/day02.txt"))

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn valid-asc? [x y]
  (and (< x y) (< (- y x) 4)))

(defn valid-desc? [x y]
  (and (> x y) (< (- x y) 4)))

(defn valid-level?
  [valid? [b i x] y]
  (if (valid? x y)
    [true (inc i) y]
    (reduced [false i x])))

(defn first-fault [[x & xs]]
  (let [f (if (> x (first xs)) valid-desc? valid-asc?)]
    (reduce (partial valid-level? f) [true 0 x] xs)))

(defn count-safe-reports [safe? n report]
  (if (safe? report) (inc n) n))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce
         (completing (partial count-safe-reports
                        #(first (first-fault %))))
         0 (line-seq rdr)))))

(defn acceptable-report? [report]
  (let [[valid? idx] (first-fault report)]
    (cond
      valid? true

      (and (not valid?) (zero? idx))
      (or (first (first-fault (rest report)))
          (first (first-fault (util/pluck report (inc idx)))))

      :else
      (or (first (first-fault (util/pluck report (dec idx))))
          (first (first-fault (util/pluck report idx)))
          (first (first-fault (util/pluck report (inc idx))))))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce
         (completing (partial count-safe-reports acceptable-report?))
         0 (line-seq rdr)))))
