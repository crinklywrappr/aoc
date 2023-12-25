(ns crinklywrappr.aoc.2023.day17
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.graph :as g]))

(def file (io/resource "2023/day17.txt"))

(def lines (with-open [rdr (io/reader file)]
             (mapv vec (line-seq rdr))))

(def max-row (count lines))

(def max-col (count (first lines)))

(defrecord Node [row col block dir]
  g/INode
  (id [_] [row col block dir]))

(defrecord Edge [from to heat-loss]
  g/IEdge
  (parent [_] from)
  (child [_] to))

(defrecord Path [graph total-heat-loss]
  g/IPath
  (graph-at-node [_] graph))

(defn p1children [{:keys [row col dir block]}]
  (filterv
   (fn valid? [{:keys [col row block]}]
     (and (< -1 row) (< row max-row)
          (< -1 col) (< col max-col)
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

(defn part1 []
  (->>
   (g/graph
    p1children
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
    (map->Node {:row 0 :col 0}))
   g/dijkstra
   (filter (fn [[[row col] _]] (and (== row (dec max-row)) (== col (dec max-col)))))
   (apply min-key (comp :total-heat-loss val)) val :total-heat-loss))

(defn p2children [{:keys [row col dir block]}]
  (filterv
   (fn valid? [{:keys [col row block]}]
     (and (< -1 row) (< row max-row)
          (< -1 col) (< col max-col)
          (pos? block)))
   (case dir
     :east [(->Node row (inc col) (dec block) :east)
            (->Node (+ 4 row) col 6 :north)
            (->Node (+ 4 row) col 6 :south)]
     :south [(->Node (inc row) col (dec block) :south)
             (->Node row (+ 4 col) 6 :east)
             (->Node row (+ 4 col) 6 :west)]
     :west [(->Node row (inc col) (dec block) :west)
            (->Node (+ 4 row) col 6 :south)
            (->Node (+ 4 row) col 6 :north)]
     :north [(->Node (inc row) col (dec block) :north)
             (->Node row (+ 4 col) 6 :west)
             (->Node row (+ 4 col) 6 :east)]
     [(->Node 0 5 6 :east)
      (->Node 5 0 6 :south)])))

(defn visualize [{:keys [edges]}]
  (reduce
   (fn [a {{:keys [row col]} :to}]
     (assoc-in a [row col] \#))
   (assoc-in lines [0 0] \#) edges))

(defn row-weight [col orig-row row]
  (reduce
   (fn [a b] (+ a (- (byte (get b col)) 48)))
   0 (subvec lines (inc (min orig-row row)) (inc (max orig-row row)))))

(defn col-weight [row orig-col col]
  (reduce
   (fn [a b] (+ a (- (byte b) 48)))
   0 (subvec (lines row) (inc (min orig-col col)) (inc (max orig-col col)))))

(defn part2 []
  (->>
   (g/graph
    p2children
    (fn edges [{orig-row :row orig-col :col :as parent} {:keys [row col] :as child}]
      (cond
        (> (abs (- row orig-row)) 1) [(->Edge parent child (row-weight col orig-row row))]
        (> (abs (- col orig-col)) 1) [(->Edge parent child (col-weight row orig-col col))]
        :else [(->Edge parent child (- (byte (-> lines (get row) (get col))) 48))]))
    (fn make-path
      ([graph] (->Path graph 0))
      ([{:keys [total-heat-loss]} graph {:keys [heat-loss]}]
       (->Path graph (+ total-heat-loss heat-loss))))
    (fn path-comparator [{a :total-heat-loss} {b :total-heat-loss}]
      (cond
        (== a b) 0
        (< a b) -1
        :else 1))
    (map->Node {:row 0 :col 0}))
   g/dijkstra
   (filter (fn [[[row col] _]] (and (== row (dec max-row)) (== col (dec max-col)))))
   (apply min-key (comp :total-heat-loss val)) val :total-heat-loss))

;; 1460 <= too high
;; 1443 <= too high
