(ns crinklywrappr.aoc.2024.day16
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [crinklywrappr.aoc.graph :as g]))

(def file (io/resource "2024/day16.txt"))

(def gsource
  (with-open [rdr (io/reader file)]
    (mapv vec (line-seq rdr))))

(defrecord Node [coord dir cost]
  g/INode
  (id [_] [coord dir cost]))

(defrecord Edge [from to]
  g/IEdge
  (parent [_] from)
  (child [_] to))

(defrecord Path [graph score]
  g/IPath
  (graph-at-node [_] graph))

(defn element-at [[row col]]
  (get-in gsource [row col]))

(defn next-coord [[row col] [dr dc]]
  [(+ row dr) (+ col dc)])

(defn get-turns [coord dir]
  (case dir
    [-1 0] [(->Node coord [0 -1] 1000)
            (->Node coord [0 1] 1000)]
    [1 0] [(->Node coord [0 -1] 1000)
           (->Node coord [0 1] 1000)]
    [0 -1] [(->Node coord [-1 0] 1000)
            (->Node coord [1 0] 1000)]
    [0 1] [(->Node coord [-1 0] 1000)
           (->Node coord [1 0] 1000)]))

(defn pathfinder []
  (let [start [(- (count gsource) 2) 1]]
    (g/graph
     (fn children [{:keys [coord dir cost]}]
       (let [turns (get-turns coord dir)
             coord' (next-coord coord dir)]
         (if (not= \# (element-at coord'))
           (conj turns (->Node coord' dir 1))
           turns)))
     (fn edges [parent child]
       [(->Edge parent child)])
     (fn make-path
       ([graph] (->Path graph 0))
       ([{:keys [score]} graph _]
        (->Path graph (+ score (:cost (zip/node graph))))))
     (fn cmp [{a :score} {b :score}]
       (cond
         (< a b) -1
         (> a b) 1
         :else 0))
     (->Node start [0 1] 0))))

(defn part1 []
  (let [end [1 (- (count (first gsource)) 2)]]
    (->> (pathfinder)
         g/dijkstra
         (filter (fn [[[coord & _] & _]] (= coord end)))
         (map (comp :score second))
         (apply min))))
