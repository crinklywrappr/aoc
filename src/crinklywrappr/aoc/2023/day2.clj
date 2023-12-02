(ns crinklywrappr.aoc.2023.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]))

(def file (io/resource "2023/day2.txt"))

(defn parse-line [line]
  (let [[game record] (sg/split line #":")
        game (parse-long (re-find #"\d+" game))
        reveals (sg/split record #";")
        parse (fn [[count color]] [(parse-long count) (keyword color)])
        parsed-reveals (mapcat (comp (partial map (comp parse rest))
                                  (partial re-seq #"(\d+) ([^,]+)"))
                               reveals)]
    [game parsed-reveals]))

;; Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes.
(defn below-threshold? [[count color]]
  (cond
    (= color :red) (<= count 12)
    (= color :green) (<= count 13)
    (= color :blue) (<= count 14)))

(defn possible-game? [[game reveals]]
  (every? below-threshold? reveals))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (comp (map parse-line)
           (filter possible-game?)
           (map first))
        (transduce + (line-seq rdr)))))

(defn power [[_ reveals]]
  (->> reveals
       (reduce
        (fn [maximums [count color]]
          (if (> count (get maximums color))
            (assoc maximums color count)
            maximums))
        {:red 0 :green 0 :blue 0})
       vals (apply *)))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp power parse-line))
        (transduce + (line-seq rdr)))))
