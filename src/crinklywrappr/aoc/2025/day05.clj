(ns crinklywrappr.aoc.2025.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]))

(def file (io/resource "2025/day05.txt"))

(defn parse-line [line]
  (cond
    (sg/includes? line "-")
    (->> (sg/split line #"-")
         (mapv parse-long))
    (seq line) (parse-long line)))

(defn maybe-merge [[a b] [c d]]
  (cond
    (<= a c b) [a (max b d)]
    (<= c a d) [c (max b d)]
    (<= a d b) [(min a c) b]
    (<= c b d) [(min a c) d]
    (== (inc b) c) [a d]
    (== (inc d) a) [c b]))

(defn merge-ranges
  ([ranges]
   (loop [merged-ranges (list) [subject & candidates] ranges]
     (if (nil? subject)
       merged-ranges
       (let [[merged-range shorter-candidates] (merge-ranges subject candidates)]
         (recur (conj merged-ranges merged-range) shorter-candidates)))))
  ([subject candidates]
   (loop [subject subject rejections (list)
          [candidate & candidates] candidates]
     (if (nil? candidate)
       [subject rejections]
       (let [new-subject (maybe-merge subject candidate)]
         (cond
           (nil? new-subject) (recur subject (conj rejections candidate) candidates)
           (some? new-subject) (recur new-subject (list) (concat rejections candidates))))))))

(defn fresh? [ranges ingredient]
  (some (fn [[from to]] (<= from ingredient to)) ranges))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing
      (fn [[ranges n] x]
        (cond
          (vector? x) [(conj ranges x) 0]
          (nil? x) [(merge-ranges ranges) 0]
          :else [ranges (if (fresh? ranges x) (inc n) n)]))
      (fn [[_ n]] n))
     [(list) 0] (line-seq rdr))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing
      (fn [ranges x]
        (if (vector? x)
          (conj ranges x)
          (reduced (merge-ranges ranges))))
      (fn [ranges]
        (-> (map (fn [[from to]] (inc (- to from))))
            (transduce + ranges))))
     (list) (line-seq rdr))))
