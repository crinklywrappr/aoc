(ns crinklywrappr.aoc.2015.day3
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2015/day3.txt"))

(defn move
  [[x y :as coords] character]
  (cond
    (= character \^) [x (dec y)]
    (= character \v) [x (inc y)]
    (= character \>) [(inc x) y]
    (= character \<) [(dec x) y]
    :else coords))

(defn part1 []
  (letfn [(visit [[visited coords] character]
            (let [new-coords (move coords character)]
              [(conj visited new-coords) new-coords]))]
    (with-open [rdr (io/reader file)]
      (->> (util/char-seq rdr)
           (reduce visit [#{[0 0]} [0 0]])
           first count))))

(defn part2 []
  (letfn [(visit [[visited coords selector] character]
            (let [new-coords (move (get coords selector) character)]
              [(conj visited new-coords)
               (assoc coords selector new-coords)
               (* selector -1)]))]
    (with-open [rdr (io/reader file)]
      (->> (util/char-seq rdr)
           (reduce visit [#{[0 0]} {1 [0 0] -1 [0 0]} 1])
           first count))))
