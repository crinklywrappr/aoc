(ns crinklywrappr.aoc.2024.day08
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2024/day08.txt"))
(def sz 50)

(defn parse-line [row line]
  [row (util/re-pos #"[^.]" line)])

(defn add-antenna [row state [col antenna]]
  (update state antenna (fnil conj []) [row col]))

(defn build-state [state [row antennas]]
  (reduce (partial add-antenna row) state antennas))

(defn count-antinodes [locs [_ antennas]]
  (->>
   (for [[r c :as a] antennas
         [r' c' :as b] antennas
         :let [dr (- r' r) dc (- c' c)
               [nr nc :as loc] [(+ r (* dr 2)) (+ c (* dc 2))]]
         :when (and (not= a b) (> nr -1) (< nr sz) (> nc -1) (< nc sz))]
     loc)
   (into locs)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map-indexed parse-line)
                    (completing build-state)
                    {})
         (reduce count-antinodes #{})
         count)))

(defn count-resonant-antinodes [locs [_ antennas]]
  (->>
   (for [[r c :as a] antennas
         [r' c' :as b] antennas
         :let [dr (- r' r) dc (- c' c)]
         :when (not= a b)]
     (iteration
      (fn next-resonance [[nr nc]]
        [(+ nr dr) (+ nc dc)])
      :somef (fn end? [[nr nc]]
               (and (> nr -1) (< nr sz) (> nc -1) (< nc sz)))
      :initk [r c]))
   (reduce into locs)))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map-indexed parse-line)
                    (completing build-state)
                    {})
         (reduce count-resonant-antinodes #{})
         count)))
