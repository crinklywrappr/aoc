(ns crinklywrappr.aoc.2023.day17
  (:require [clojure.zip :as zip]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.graph :as g]))

;; (require '[clj-async-profiler.core :as prof])

(def file (io/resource "2023/day17ex.txt"))

(defrecord Node [row col block dir]
  g/INode
  (id [_] {:row row :col col :block block :dir dir}))

(defrecord Edge [from to heat-loss]
  g/IEdge
  (parent [_] from)
  (child [_] to))

(defrecord Path [graph total-heat-loss]
  g/IPath
  (graph-at-node [_] graph))

(defn children [max-row max-col {:keys [row col dir block]}]
  (filterv
   (fn valid? [{:keys [col row block]}]
     (and (< -1 row max-row)
          (< -1 col max-col)
          (pos? block)))
   (case dir
     :east [(->Node (dec row) col 3 :north)
            (->Node (inc row) col 3 :south)
            (->Node row (inc col) (dec block) :east)]
     :south [(->Node row (inc col) 3 :east)
             (->Node row (dec col) 3 :west)
             (->Node (inc row) col (dec block) :south)]
     :west [(->Node (inc row) col 3 :south)
            (->Node (dec row) col 3 :north)
            (->Node row (dec col) (dec block) :west)]
     :north [(->Node row (dec col) 3 :west)
             (->Node row (inc col) 3 :east)
             (->Node (dec row) col (dec block) :north)]
     nil [(->Node 0 1 3 :east)
          (->Node 1 0 3 :south)])))

(defn visualize [lines {:keys [edges]}]
  (reduce
   (fn [a {{:keys [row col]} :to}]
     (assoc-in a [row col] \#))
   (assoc-in lines [0 0] \#) edges))

(defn part1 []
  (let [lines (with-open [rdr (io/reader file)]
                (mapv vec (line-seq rdr)))
        max-row (count lines)
        max-col (count (first lines))]
    (->>
     (g/graph
      (partial children max-row max-col)
      (fn edges [parent {:keys [row col] :as child}]
        [(->Edge parent child (- (byte (-> lines (get row) (get col))) 48))])
      (fn make-path
        ([graph] (->Path graph 0))
        ([{:keys [total-heat-loss]} graph {:keys [heat-loss]}]
         (->Path graph (+ total-heat-loss heat-loss))))
      (fn path-comparator [{a :total-heat-loss} {b :total-heat-loss}]
        (cond
          (== a b) 0
          (< a b) -1
          :else 1))
      (map->Node {:row 0 :col 0 :block 4}))
     g/dijkstra
     (filter (fn [[{:keys [row col]} _]] (and (== row (dec max-row)) (== col (dec max-col)))))
     (apply min-key (comp :total-heat-loss val))
     val :total-heat-loss)))
