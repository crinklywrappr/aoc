(ns crinklywrappr.aoc.2025.day04
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2025/day04.txt"))

;; (def row-sz 10)
(def row-sz 138)

(def boundary (apply str (repeat row-sz \x)))

(defn format-row [row]
  (vec (str "x" row "x")))

(defn paper-accessible? [rows col]
  (let [m (frequencies
           (concat
            (-> (rows 0) (subvec (dec col) (+ 2 col)))
            (-> (rows 1) (subvec (dec col) (+ 2 col)))
            (-> (rows 2) (subvec (dec col) (+ 2 col)))))]
    (< (dec (get m \@)) 4)))

(defn check-row [total rows]
  (loop [total total col 1]
    (let [spot (get-in rows [1 col])]
      (cond
        (= spot \x) total
        (= spot \@) (recur (if (paper-accessible? rows col) (inc total) total) (inc col))
        :else (recur total (inc col))))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (as-> [boundary] $
      (util/wrap-line-seq rdr $ $)
      (map format-row $)
      (partition 3 1 $)
      (map vec $)
      (reduce check-row 0 $))))

(defn clear-row [[total room] rows]
  (loop [total' total row (rows 1) col 1]
    (let [spot (get-in rows [1 col])]
      (cond
        (= spot \x) [total' (conj room row)]

        (and (= spot \@) (paper-accessible? rows col))
        (recur (inc total') (assoc row col \.) (inc col))

        :else (recur total' row (inc col))))))

(defn clear-room [room]
  (transduce
   (map vec)
   (completing clear-row (fn [[total room]] [total (conj room (format-row boundary))]))
   [0 [(format-row boundary)]] (partition 3 1 room)))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (loop [total 0 room (as-> [boundary] $
                          (util/wrap-line-seq rdr $ $)
                          (map format-row $))]
      (let [[total' room'] (clear-room room)]
        (if (zero? total')
          total
          (recur (+ total total') room'))))))
