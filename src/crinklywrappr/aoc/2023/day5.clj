(ns crinklywrappr.aoc.2023.day5
  (:require [clojure.string :as sg]
            [clojure.java.io :as io]))

(def file (io/resource "2023/day5.txt"))

(defn advance [lines]
  (rest (drop-while #(and (some? %) (not (sg/ends-with? % ":"))) lines)))

(defn locations [lookup-values read-seeds]
  (with-open [rdr (io/reader file)]
    (let [[line & lines] (line-seq rdr)]
      (loop [[found not-found] [[] (read-seeds line)]
             [line & lines] (drop 2 lines)]
        (cond
          (nil? line) (concat found not-found)
          (empty? line) (recur [[] (concat found not-found)] (rest lines))
          (empty? not-found) (recur [[] (concat found not-found)] (advance lines))
          :else (let [[dst src len] (mapv parse-long (re-seq #"\d+" line))]
                  (recur (reduce (lookup-values dst src len) [found []] not-found) lines)))))))

(defn part1 []
  (->>
   (locations
    (fn lookup-values [dst src len]
      (fn lookup-values' [[found not-found] n]
        (if (and (<= src n) (< n (+ src len)))
          [(conj found (- n (- src dst))) not-found]
          [found (conj not-found n)])))
    (fn read-seeds [line]
      (mapv parse-long (re-seq #"\d+" line))))
   (apply min)))

(defn lookup-values [dst src len]
  (fn lookup-values' [[found not-found] [n dim]]
    (cond
      (and (>= n src) (<= (+ n dim) (+ src len))) ;; straight conversion
      [(conj found [(- n (- src dst)) dim]) not-found]

      (and (< n src (+ n dim)) (<= (+ n dim) (+ src len))) ;; left side out, right side in
      [(conj found [dst (- (+ n dim) src)]) (conj not-found [n (- src n)])]

      (and (<= src n) (< n (+ src len) (+ n dim))) ;; left side in, right side out
      [(conj found [(- n (- src dst)) (- (+ src len) n)]) (conj not-found [(+ src len) (- (+ n dim) (+ src len))])]

      (and (< n src) (> (+ n dim) (+ src len))) ;; three-way split
      [(conj found [dst len]) (conj not-found [n (- src n)] [(+ src len) (- (+ n dim) (+ src len))])]

      :else [found (conj not-found [n dim])])))

(defn part2 []
  (->>
   (locations
    lookup-values
    (fn read-seeds [line]
      (->> (re-seq #"\d+" line)
           (map parse-long)
           (partition 2))))
   (apply min-key first)
   first))
