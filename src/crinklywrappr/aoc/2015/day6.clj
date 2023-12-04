(ns crinklywrappr.aoc.2015.day6
  (:require [clojure.java.io :as io]))

(def file (io/resource "2015/day6.txt"))

(def rgx #"toggle|off|on|\d+")

(defn change-row [f from to row]
  (vec
   (concat
    (subvec row 0 from)
    (mapv f (subvec row from (inc to)))
    (subvec row (inc to)))))

(defn change-grid [grid [action fx from tx to]]
  (vec
   (concat
    (subvec grid 0 from)
    (->> (subvec grid from (inc to))
         (mapv (partial change-row action fx tx)))
    (subvec grid (inc to)))))

(defn part1 []
  (letfn [(parse-line [line]
            (->> (re-seq rgx line)
                 ((fn [[action & coords]]
                    (cons (condp = action
                            "off" (constantly -1)
                            "on" (constantly 1)
                            "toggle" (partial * -1))
                          (mapv parse-long coords))))))]
    (with-open [rdr (io/reader file)]
      (let [grid (vec (repeat 1000 (vec (repeat 1000 -1))))]
        (->> rdr line-seq
             (mapv parse-line)
             (reduce change-grid grid)
             (mapcat identity)
             (filter pos?)
             count)))))

(defn part2 []
  (letfn [(parse-line [line]
            (->> (re-seq rgx line)
                 ((fn [[action & coords]]
                    (cons (condp = action
                            "off" (fn [brightness] (max 0 (dec brightness)))
                            "on" inc
                            "toggle" (partial + 2))
                          (mapv parse-long coords))))))]
    (with-open [rdr (io/reader file)]
      (let [grid (vec (repeat 1000 (vec (repeat 1000 0))))]
        (->> rdr line-seq
             (mapv parse-line)
             (reduce change-grid grid)
             (mapcat identity)
             (apply +))))))
