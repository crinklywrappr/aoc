(ns crinklywrappr.aoc.2015.day2
  (:require [clojure.java.io :as io]))

(def file (io/resource "2015/day2.txt"))

(defn parse [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn wrap [[l w h :as dim]]
  (let [[x y _] (sort < dim)]
    (->> (+ (* l w) (* w h) (* h l))
         (* 2) (+ (* x y)))))

(defn part1 []
  (letfn []
    (with-open [rdr (io/reader file)]
      (-> (map (comp wrap parse))
          (transduce + (line-seq rdr))))))

(defn bow [dim]
  (let [[x y z] (sort < dim)]
    (+ x x y y (* x y z))))

(defn part2 []
  (letfn []
    (with-open [rdr (io/reader file)]
      (-> (map (comp bow parse))
          (transduce + (line-seq rdr))))))
