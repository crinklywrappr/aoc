(ns crinklywrappr.aoc.2023.day9
  (:require [clojure.java.io :as io]))

(def file (io/resource "2023/day9.txt"))

(defn parse-line [line]
  (mapv parse-long (.split line " ")))

(defn rsub
  ([a b] (- b a))
  ([[a b]] (- b a)))

(defn all-zero? [nums]
  (not-every? zero? nums))

(defn changes [nums]
  (mapv rsub (partition 2 1 nums)))

(defn continue [nums]
  (->> (iteration changes :somef all-zero? :vf last :initk nums)
       (reduce + (last nums))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp continue parse-line))
        (transduce + (line-seq rdr)))))

(defn rcontinue [nums]
  (->> (iteration changes :somef all-zero? :vf first :initk nums)
       reverse (reduce rsub) (- (first nums))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp rcontinue parse-line))
        (transduce + (line-seq rdr)))))
