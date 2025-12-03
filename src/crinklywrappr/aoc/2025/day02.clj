(ns crinklywrappr.aoc.2025.day02
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as sg]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2025/day02.txt"))

(defn parse-token [token]
  (mapv parse-long (sg/split token #"-")))

(defn magnitude [x]
  (long (inc (math/floor (math/log10 x)))))

(defn generate-invalid-ids [len target-len]
  (for [xs (combo/selections (range 10) len)
        :when (pos? (first xs))]
    (as-> (quot target-len len) $
      (repeat $ xs)
      (apply concat $)
      (apply str $)
      (parse-long $))))

(defn invalid-ids-between [from to]
  (for [mag (filter even? (range (magnitude from) (inc (magnitude to))))
        id (generate-invalid-ids (/ mag 2) mag)
        :when (and (>= id from) (<= id to))]
    id))

(defn more-invalid-ids-between [from to]
  (distinct
   (for [mag (range (max 2 (magnitude from)) (inc (magnitude to)))
         len (filter #(zero? (mod mag %)) (range 1 (inc (quot mag 2))))
         id (generate-invalid-ids len mag)
         :when (and (>= id from) (<= id to))]
     id)))

(defn sum-invalid-ids [total [from to]]
  (apply + total (invalid-ids-between from to)))

(defn sum-more-invalid-ids [total [from to]]
  (apply + total (more-invalid-ids-between from to)))

(defn part1 []
  (with-open [rdr (util/delimiter-reader file)]
    (transduce
     (map parse-token)
     (completing sum-invalid-ids)
     0 (util/token-seq rdr))))

(defn part2 []
  (with-open [rdr (util/delimiter-reader file)]
    (transduce
     (map parse-token)
     (completing sum-more-invalid-ids)
     0 (util/token-seq rdr))))
