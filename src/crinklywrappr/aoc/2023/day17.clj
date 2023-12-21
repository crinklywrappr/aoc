(ns crinklywrappr.aoc.2023.day17
  (:require [clojure.zip :as zip]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.graph :as g]))

(def file (io/resource "2023/day17ex.txt"))

(defrecord Node [row col block dir]
  g/INode
  (id [_] {:row row :col col :block block :dir dir}))

(defrecord Edge [from to heat-loss]
  g/IEdge
  (parent [_] from)
  (child [_] to))

(defn children [max-row max-col {:keys [row col dir block]}]
  (if (nil? dir)
    [(map->Node {:row 0 :col 1 :block 3 :dir :east})
     (map->Node {:row 1 :col 0 :block 3 :dir :south})]
    (filterv
     (fn valid? [{:keys [col row block]}]
       (and (< -1 row max-row)
            (< -1 col max-col)
            (pos? block)))
     (case dir
       :east [(map->Node {:row (dec row) :col col :block 3 :dir :north})
              (map->Node {:row (inc row) :col col :block 3 :dir :south})
              (map->Node {:row row :col (inc col) :block (dec block) :dir :east})]
       :south [(map->Node {:row row :col (inc col) :block 3 :dir :east})
              (map->Node {:row row :col (dec col) :block 3 :dir :west})
              (map->Node {:row (inc row) :col col :block (dec block) :dir :south})]
       :west [(map->Node {:row (inc row) :col col :block 3 :dir :south})
               (map->Node {:row (dec row) :col col :block 3 :dir :north})
               (map->Node {:row row :col (dec col) :block (dec block) :dir :west})]
       :north [(map->Node {:row row :col (dec col) :block 3 :dir :west})
               (map->Node {:row row :col (inc col) :block 3 :dir :east})
               (map->Node {:row (dec row) :col col :block (dec block) :dir :north})]))))

(defn visualize [lines {:keys [edges]}]
  (reduce
   (fn [a {{:keys [row col]} :to}]
     (assoc-in a [row col] \#))
   (assoc-in lines [0 0] \#) edges))

(defn part1 []
  (let [lines (with-open [rdr (io/reader file)]
                (mapv vec (line-seq rdr)))
        max-row (count lines)
        max-col (count (first lines))
        mygraph (g/graph (partial children max-row max-col)
                         (fn edges [parent {:keys [row col] :as child}]
                           [(->Edge parent child (- (byte (get-in lines [row col])) 48))])
                         (fn path-comparator [a b]
                           (letfn [(agg [n {:keys [heat-loss]}]
                                     (+ n heat-loss))]
                             (let [x (reduce agg 0 a) y (reduce agg 0 b)]
                               (cond
                                 (== x y) 0
                                 (< x y) -1
                                 :else 1))))
                         (map->Node {:row 0 :col 0 :block 4}))
        distances (g/dijkstra mygraph)]
    (assert (== (count distances) 1717))
    (println (->> distances
                  (filter (fn [[{:keys [row col]} _]] (and (== row 12) (== col 12))))
                  (mapv (comp (partial apply +) (partial mapv :heat-loss) :edges val))))
    (assert (= {104 2, 115 1, 106 1, 114 1, 102 1}
               (->> distances
                    (filter (fn [[{:keys [row col]} _]] (and (== row 12) (== col 12))))
                    (mapv (comp (partial apply +) (partial mapv :heat-loss) :edges val)) frequencies)))
    (->> distances
         (filter (fn [[{:keys [row col]} _]] (and (== row 12) (== col 12))))
         (apply min-key (comp (partial apply +) (partial mapv :heat-loss) :edges val))
         second (visualize lines))))
