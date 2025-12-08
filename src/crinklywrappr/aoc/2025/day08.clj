(ns crinklywrappr.aoc.2025.day08
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.set :as st]))

(def file (io/resource "2025/day08.txt"))
(def p1-limit 999) ;; 9 for example file

(defn parse-line [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn distance [[[px py pz] [qx qy qz]]]
  (abs
   (math/sqrt
    (+ (math/pow (- qx px) 2)
       (math/pow (- qy py) 2)
       (math/pow (- qz pz) 2)))))

(defn pair [[pairs boxes] box]
  [(concat pairs (map (partial vector box) boxes))
   (conj boxes box)])

(defn find-circuits [circuits box1 box2]
  (reduce
   (fn [[circuit1 circuit2] circuit]
     (let [one? (contains? circuit box1)
           two? (contains? circuit box2)]
       (cond
         (and one? two?) (reduced [circuit circuit])
         (and one? (some? circuit2)) (reduced [circuit circuit2])
         (and two? (some? circuit1)) (reduced [circuit1 circuit])
         one? [circuit nil]
         two? [nil circuit]
         :else [circuit1 circuit2])))
   [nil nil] circuits))

(defn add-junction [circuits [box1 box2]]
  (let [[circuit1 circuit2] (find-circuits circuits box1 box2)]
    (cond
      (and (nil? circuit1) (nil? circuit2)) (conj circuits #{box1 box2})
      (nil? circuit1) (-> (disj circuits circuit2)
                          (conj (conj circuit2 box1)))
      (nil? circuit2) (-> (disj circuits circuit1)
                          (conj (conj circuit1 box2)))
      (not= circuit1 circuit2) (-> (disj circuits circuit1 circuit2)
                                   (conj (st/union circuit1 circuit2)))
      :else circuits)))

(defn solve1 [[pairs _]]
  (->> (sort-by distance < pairs)
       (reductions add-junction #{})
       (drop p1-limit) first
       (sort-by count >)
       (transduce (comp (map count) (take 3)) *)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing pair solve1)
     [[] [(parse-line (.readLine rdr))]]
     (line-seq rdr))))

(defn final-box [box-count]
  (fn [circuits [box1 box2 :as distance]]
    (let [new-circuits (add-junction circuits distance)]
      (if (and (== (count new-circuits) 1)
               (== (count (first new-circuits)) box-count))
        (let [[px _ _] box1 [qx _ _] box2]
          (reduced (* px qx)))
        new-circuits))))

(defn solve2 [[pairs boxes]]
  (->> (sort-by distance < pairs)
       (reduce (final-box (count boxes)) #{})))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing pair solve2)
     [[] [(parse-line (.readLine rdr))]]
     (line-seq rdr))))
