(ns crinklywrappr.aoc.2015.day06
  (:require [clojure.java.io :as io]))

(def file (io/resource "2015/day06.txt"))

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

(defn parse-line [line & {:keys [on off toggle]}]
  (->> (re-seq rgx line)
       ((fn [[action & coords]]
          (cons (condp = action
                  "on" on
                  "off" off
                  "toggle" toggle)
                (mapv parse-long coords))))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (let [grid (vec (repeat 1000 (vec (repeat 1000 -1))))]
      (->> rdr line-seq
           (mapv #(parse-line
                   % :on (constantly 1)
                   :off (constantly -1)
                   :toggle (partial * -1)))
           (reduce change-grid grid)
           (mapcat identity)
           (filter pos?)
           count))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (let [grid (vec (repeat 1000 (vec (repeat 1000 0))))]
      (->> rdr line-seq
           (mapv #(parse-line
                   % :on inc
                   :off (fn [brightness]
                          (max 0 (dec brightness)))
                   :toggle (partial + 2)))
           (reduce change-grid grid)
           (mapcat identity)
           (apply +)))))
