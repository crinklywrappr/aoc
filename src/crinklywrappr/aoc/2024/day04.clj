(ns crinklywrappr.aoc.2024.day04
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2024/day04.txt"))
(def len 140)

(defn padding []
  (repeat 3 (apply str (repeat len \.))))

(defn check-horizontal [i [l1 & _]]
  (let [w (str (l1 (- i 3))
               (l1 (- i 2))
               (l1 (- i 1))
               (l1 i))]
    (or (= w "XMAS") (= w "SAMX"))))

(defn check-vertical [i [l1 l2 l3 l4]]
  (let [w (str (l1 i) (l2 i) (l3 i) (l4 i))]
    (or (= w "XMAS") (= w "SAMX"))))

(defn check-southwest [i [l1 l2 l3 l4]]
  (let [w (str (l1 i)
               (l2 (- i 1))
               (l3 (- i 2))
               (l4 (- i 3)))]
    (or (= w "XMAS") (= w "SAMX"))))

(defn check-northwest [i [l1 l2 l3 l4]]
  (let [w (str (l4 i)
               (l3 (- i 1))
               (l2 (- i 2))
               (l1 (- i 3)))]
    (or (= w "XMAS") (= w "SAMX"))))

(defn check-directions [lines total i]
  (reduce
   (fn [a b] (if b (inc a) a))
   total
   [(check-horizontal i lines)
    (check-southwest i lines)
    (check-northwest i lines)
    (check-vertical i lines)]))

(defn check-one-direction [f lines total i]
  (if (f i lines) (inc total) total))

(defn check-lines [total lines]
  (if (= ((last lines) 0) \.)
    (reduce (partial check-one-direction check-horizontal lines) total (range 3 len))
    (+ total
       (reduce (partial check-directions lines) 0 (range 3 len))
       (reduce (partial check-one-direction check-vertical lines) 0 (range 0 3)))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (->> (util/wrap-line-seq rdr (padding))
         (map vec)
         (partition 4 1)
         (reduce check-lines 0))))

(defn check-xmas [i [l1 l2 l3]]
  (let [x1 (str (l1 (dec i))
                (l2 i)
                (l3 (inc i)))
        x2 (str (l3 (dec i))
                (l2 i)
                (l1 (inc i)))]
    (and (or (= x1 "MAS") (= x1 "SAM"))
         (or (= x2 "MAS") (= x2 "SAM")))))

(defn count-xmas [total lines]
  (reduce (partial check-one-direction check-xmas lines) total (range 1 (dec len))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (map vec)
         (partition 3 1)
         (reduce count-xmas 0))))
