(ns crinklywrappr.aoc.2024.day14
  (:require [clojure.java.io :as io]))

(def file (io/resource "2024/day14.txt"))
(def rows 103)
(def cols 101)

(defn parse-line [line]
  (->> (re-seq #"-?\d+" line)
       (mapv parse-long)))

(defn calculate-position [[col row vcol vrow]]
  (loop [row' row col' col second 0]
    (if (== second 100)
      [row' col']
      (recur (mod (+ row' vrow) rows)
             (mod (+ col' vcol) cols)
             (inc second)))))

(let [mid-row (/ (dec rows) 2)
      mid-col (/ (dec cols) 2)]
  (defn count-quadrants [state [row col]]
    (if (or (== row mid-row) (== col mid-col))
      state
      (case [(< row mid-row) (< col mid-col)]
        [true true] (update state 0 inc)
        [true false] (update state 1 inc)
        [false true] (update state 2 inc)
        [false false] (update state 3 inc)))))

(defn render-map [[second _ positions]]
  (let [ps (frequencies positions)]
    (doseq [row (range rows)]
      (doseq [col (range cols)]
        (print (get ps [row col] \.)))
      (println)))
  second)

(defn part1 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map (comp calculate-position parse-line))
                    (completing count-quadrants)
                    [0 0 0 0])
         (apply *))))

(defn generations [positions]
  (letfn [(next-position [[row col vrow vcol]]
            [(mod (+ row vrow) rows)
             (mod (+ col vcol) cols)
             vrow vcol])]
    (iteration
     (fn [[seconds positions]]
       [(inc seconds) (mapv next-position positions)])
     :vf (fn [[seconds positions]] [seconds (mapv (fn [[row col & _]] [row col]) positions)])
     :initk [0 (mapv (fn [[px py vx vy]] [py px vy vx]) positions)])))

(defn average-delta [[second xs]]
  [second
   (as-> (for [[i [r c]] (map-indexed vector xs)
               [h [r' c']] (map-indexed vector xs)
               :when (not= i h)]
           (+ (abs (- r' r)) (abs (- c' c)))) $
     (apply + $) (/ $ (* (count xs) (count xs))))
   xs])

(defn part2 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (mapv parse-line)
         generations
         (take (* rows cols))
         (mapv average-delta)
         (sort-by second <)
         first
         render-map)))
