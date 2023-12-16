(ns crinklywrappr.aoc.2023.day15
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day15.txt"))

(defn calc-score [score n]
  (rem (* 17 (+ score n)) 256))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (reduce
     (fn f [[total score] c]
       (case c
         \newline (reduced (+ total score))
         \, [(+ total score) 0]
         [total (calc-score score (byte c))]))
     [0 0] (util/char-seq rdr))))

(defn focusing-power [labels]
  (fn focusing-power' [[total box idx] label]
    (let [focal-length (get labels label)]
      [(+ total (* (inc box) (inc idx) focal-length)) box (inc idx)])))

(defn sum-focusing-power [labels]
  (fn sum-focusing-power' [total [box lenses]]
    (first (reduce (focusing-power labels) [total box 0] lenses))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (reduce
     (fn f [[labels boxes label box] c]
       (case c
         \newline (reduced (reduce (sum-focusing-power labels) 0 boxes))
         \, [labels boxes label box]
         \= [labels boxes label box]
         \- [(dissoc labels label)
             (if (contains? labels label)
               (update boxes box (partial filterv #(not= label %)))
               boxes)
             "" 0]
         (let [n (byte c)]
           (if (> n 96)
             [labels boxes (str label c) (calc-score box n)]
             [(assoc labels label (- n 48))
              (cond
                (contains? labels label) boxes
                (contains? boxes box) (update boxes box conj label)
                :else (assoc boxes box [label]))
              "" 0]))))
     [{} {} "" 0] (util/char-seq rdr))))
