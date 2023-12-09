(ns crinklywrappr.aoc.2023.day9
  (:require [clojure.string :as sg]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day9.txt"))

(defn parse-line [line]
  (mapv parse-long (.split line " ")))

(defn continue [nums]
  (letfn [(rsub [[a b]] (- b a))
          (somef [nums] (not-every? zero? nums))
          (stepf [nums] (mapv rsub (partition 2 1 nums)))]
    (->> (iteration stepf :somef somef :vf last :initk nums)
         (reduce + (last nums)))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp continue parse-line))
        (transduce + (line-seq rdr)))))

(defn rcontinue [nums]
  (letfn [(rsub [[a b]] (- b a))
          (somef [nums] (not-every? zero? nums))
          (stepf [nums] (mapv rsub (partition 2 1 nums)))]
    (as-> (iteration stepf :somef somef :vf first :initk nums) $
      (reverse $) (vec $)
      (conj $ (first nums))
      (reduce (comp rsub vector) $))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp rcontinue parse-line))
        (transduce + (line-seq rdr)))))
