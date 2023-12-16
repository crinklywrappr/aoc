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

(defn focusing-power [lenses]
  (fn focusing-power' [[total idx] label]
    (let [[box focal-length] (get lenses label)]
      [(+ total (* (inc box) (inc idx) focal-length)) (inc idx)])))

(defn sum-focusing-power [lenses]
  (fn sum-focusing-power' [total box]
    (first (reduce (focusing-power lenses) [total 0] (reverse box)))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (reduce
     (fn f [[lenses boxes label box] c]
       (case c
         \newline (reduced (reduce (sum-focusing-power lenses) 0 (vals boxes)))
         \, [lenses boxes label box]
         \= [lenses boxes label box]
         \- [(dissoc lenses label)
             (if (contains? lenses label)
               (update boxes box (partial remove #(= label %)))
               boxes)
             "" 0]
         (let [n (byte c)]
           (if (> n 96)
             [lenses boxes (str label c) (calc-score box n)]
             [(assoc lenses label [box (- n 48)])
              (cond
                (contains? lenses label) boxes
                (contains? boxes box) (update boxes box conj label)
                :else (assoc boxes box (list label)))
              "" 0]))))
     [{} {} "" 0] (util/char-seq rdr))))
