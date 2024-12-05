(ns crinklywrappr.aoc.2024.day04
  (:require [clojure.java.io :as io]))

(def file (io/resource "2024/day04.txt"))

(defn check-horizontal [i lines]
  (let [l (last lines)
        w (str (l (- i 3))
               (l (- i 2))
               (l (- i 1))
               (l i))]
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

(defn check-lines [len [total l1 l2 l3] l4]
  (let [lines [l1 l2 l3 l4]]
    [(+ total
        (reduce (partial check-directions lines) 0 (range 3 len))
        (if (check-vertical 0 lines) 1 0)
        (if (check-vertical 1 lines) 1 0)
        (if (check-vertical 2 lines) 1 0))
     l2 l3 l4]))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (let [lines (map vec (line-seq rdr))
          len (count (first lines))]
      (+ (reduce (partial check-one-direction check-horizontal [(nth lines 0)]) 0 (range 3 len))
         (reduce (partial check-one-direction check-horizontal [(nth lines 1)]) 0 (range 3 len))
         (reduce (partial check-one-direction check-horizontal [(nth lines 2)]) 0 (range 3 len))
         (first (reduce (partial check-lines len) (cons 0 (take 3 lines)) (drop 3 lines)))))))

(defn check-xmas [i [l1 l2 l3]]
  (let [x1 (str (l1 (dec i))
                (l2 i)
                (l3 (inc i)))
        x2 (str (l3 (dec i))
                (l2 i)
                (l1 (inc i)))]
    (and (or (= x1 "MAS") (= x1 "SAM"))
         (or (= x2 "MAS") (= x2 "SAM")))))

(defn count-xmas [len [total l1 l2] l3]
  [(reduce (partial check-one-direction check-xmas [l1 l2 l3]) total (range 1 (dec len))) l2 l3])

(defn part2 []
  (with-open [rdr (io/reader file)]
    (let [lines (map vec (line-seq rdr))
          len (count (first lines))]
      (first (reduce (partial count-xmas len) (cons 0 (take 2 lines)) (drop 2 lines))))))
