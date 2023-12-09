(ns crinklywrappr.aoc.2023.day9
  (:require [clojure.java.io :as io]))

(def file (io/resource "2023/day9.txt"))

(defn parse-line [line]
  (mapv parse-long (.split line " ")))

(defn rsub [[a b]] (- b a))

(defn all-zero? [nums] (not-every? zero? nums))

(defn changes [nums]
  (mapv rsub (partition 2 1 nums)))

(defn continue [nums]
  (->> (iteration changes :somef all-zero?
                  :vf last :initk nums)
       (reduce + (last nums))))

(defn solve [f]
  (with-open [rdr (io/reader file)]
    (-> (map f) (transduce + (line-seq rdr)))))

(defn part1 []
  (solve (comp continue parse-line)))

(defn part2 []
  (solve (comp continue reverse parse-line)))
