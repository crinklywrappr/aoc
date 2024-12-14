(ns crinklywrappr.aoc.2022.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]))

(def file (io/resource "2022/day07.txt"))

(defn add-filesize [filesystem path size]
  (first
   (reduce
    (fn [[fs p] d]
      (let [p' (conj p d)]
        [(update fs p' (fnil + 0) size) p']))
    [filesystem []] path)))

(defn populate-filesystem [[filesystem path] line]
  (cond
    (= line "$ ls") [filesystem path]
    (= line "$ cd /") [filesystem ["/"]]
    (= line "$ cd ..") [filesystem (pop path)]
    (sg/starts-with? line "$ cd") [filesystem (conj path (.substring line 5))]
    (sg/starts-with? line "dir") [(assoc filesystem (conj path (.substring line 4)) 0) path]
    :else (let [[size _] (sg/split line #" ")]
            [(add-filesize filesystem path (parse-long size)) path])))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (reduce-kv
     (fn [a _ v] (if (< v 100000) (+ a v) a))
     0 (first (reduce populate-filesystem [{} ["/"]] (line-seq rdr))))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (let [size-map (first (reduce populate-filesystem [{} ["/"]] (line-seq rdr)))
          target (- 30000000 (- 70000000 (get size-map ["/"])))]
      (reduce-kv
       (fn [a _ v] (if (and (>= v target) (< v a)) v a))
       30000000 size-map))))
