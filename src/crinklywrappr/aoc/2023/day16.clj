(ns crinklywrappr.aoc.2023.day16
  (:require [clojure.string :as sg]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day16.txt"))

(def input
  (with-open [rdr (io/reader file)]
    (mapv identity (line-seq rdr))))

(defn char-at [input visited-obstacles row col]
  (when-let [c (get-in input [row col])]
    [c (long c) (get visited-obstacles [row col] 0)]))

(defn energize [input row col new-state]
  (update input row util/set-char-at col new-state))

(defn reflect [dir c]
  (case [dir c]
    [2r0001 \\] 2r0010
    [2r0010 \\] 2r0001
    [2r0100 \\] 2r1000
    [2r1000 \\] 2r0100
    [2r0001 \/] 2r1000
    [2r0010 \/] 2r0100
    [2r0100 \/] 2r0010
    [2r1000 \/] 2r0001))

(defn new-state [row col dir]
  (case dir
    2r0001 [row (inc col) dir]
    2r0010 [(inc row) col dir]
    2r0100 [row (dec col) dir]
    2r1000 [(dec row) col dir]))

(defn mirror? [character]
  (or (= character \\ ) (= character \/)))

(defn split? [dir c]
  (or (and (pos? (bit-and dir 2r1010)) (= c \-))
      (and (pos? (bit-and dir 2r0101)) (= c \|))))

(defn split [character]
  (case character
    \- [2r0100 2r0001]
    \| [2r1000 2r0010]))

(defn follow-ray [[input visited-obstacles rays] [row col dir]]
  (let [[c passes obstacle-passes :as info] (char-at input visited-obstacles row col)]
    (cond
      (or (nil? info) (pos? (bit-and dir obstacle-passes))
          (and (< passes 16) (pos? (bit-and dir passes))))
      [input visited-obstacles rays]

      (and (zero? obstacle-passes) (mirror? c))
      [input (assoc visited-obstacles [row col] dir) (conj rays (new-state row col (reflect dir c)))]

      (mirror? c)
      [input (update visited-obstacles [row col] bit-or dir) (conj rays (new-state row col (reflect dir c)))]

      (and (zero? obstacle-passes) (split? dir c))
      (let [[ray1 ray2] (split c)]
        [input (assoc visited-obstacles [row col] dir) (conj rays (new-state row col ray1) (new-state row col ray2))])

      (split? dir c)
      (let [[ray1 ray2] (split c)]
        [input (update visited-obstacles [row col] bit-or dir) (conj rays (new-state row col ray1) (new-state row col ray2))])

      (and (zero? obstacle-passes) (or (= c \|) (= c \-)))
      [input (assoc visited-obstacles [row col] dir) (conj rays (new-state row col dir))]

      (or (= c \|) (= c \-))
      [input (update visited-obstacles [row col] bit-or dir) (conj rays (new-state row col dir))]

      (= c \.) [(energize input row col (char dir)) visited-obstacles (conj rays (new-state row col dir))]

      :else [(energize input row col (char (bit-or dir passes))) visited-obstacles (conj rays (new-state row col dir))])))

;; 2r0001 is east, 2r0010 south, etc
(defn solve [row col dir]
  (loop [input input visited-obstacles {} rays [[row col dir]]]
    (if (empty? rays)
      (->> (sg/join input) (filterv #(< (byte %) 16)) count (+ (count visited-obstacles)))
      (let [[new-input new-visited-obstacles new-rays] (reduce follow-ray [input visited-obstacles []] rays)]
        (recur new-input new-visited-obstacles new-rays)))))

(defn part1 [] (solve 0 0 2r0001))

(defn part2 []
  (let [max-row (count input)
        max-col (count (first input))]
    (->>
     (for [row (range max-row) col (range max-col)
           :when (or (zero? row) (zero? col)
                     (== row (dec max-row))
                     (== col (dec max-col)))]
       (cond
         (and (zero? row) (zero? col))
         [[0 0 2r0001] [0 0 2r0010]]

         (and (zero? row) (== col (dec max-col)))
         [[0 col 2r0100] [0 col 2r0010]]

         (and (== row (dec max-row)) (== col (dec max-col)))
         [[row col 2r1000] [row col 2r0100]]

         (and (== row (dec max-row)) (zero? col))
         [[row 0 2r0001] [row 0 2r1000]]

         (zero? row) [[row col 2r0010]]
         (== row (dec max-row)) [[row col 2r1000]]
         (zero? col) [[row col 2r0001]]
         (== col (dec max-col)) [[row col 2r0100]]))
     (mapcat identity)
     (pmap (partial apply solve))
     (apply max))))
