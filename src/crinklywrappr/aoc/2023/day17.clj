(ns crinklywrappr.aoc.2023.day17
  (:require [clojure.zip :as zip]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.graph :as g]))

(def file (io/resource "2023/day17ex.txt"))

(defrecord Node [row col block dir]
  g/Node
  (id [_] [row col]))

(defrecord Edge [from to heat-loss]
  g/Edge
  (parent [_] from)
  (child [_] to))

(defn children [max-row max-col {:keys [row col dir block]}]
  (if (nil? dir)
    [(map->Node {:row 0 :col 1 :block 3 :dir [0 1]})
     (map->Node {:row 1 :col 0 :block 3 :dir [1 0]})]
    (filterv
     (fn valid? [{:keys [col row block]}]
       (and (< -1 row max-row)
            (< -1 col max-col)
            (pos? block)))
     (case dir
       [0 1] [(map->Node {:row (dec row) :col col :block 3 :dir [-1 0]})
              (map->Node {:row (inc row) :col col :block 3 :dir [1 0]})
              (map->Node {:row row :col (inc col) :block (dec block) :dir [0 1]})]
       [1 0] [(map->Node {:row row :col (inc col) :block 3 :dir [0 1]})
              (map->Node {:row row :col (dec col) :block 3 :dir [0 -1]})
              (map->Node {:row (inc row) :col col :block (dec block) :dir [1 0]})]
       [0 -1] [(map->Node {:row (inc row) :col col :block 3 :dir [1 0]})
               (map->Node {:row (dec row) :col col :block 3 :dir [-1 0]})
               (map->Node {:row row :col (dec col) :block (dec block) :dir [0 -1]})]
       [-1 0] [(map->Node {:row row :col (dec col) :block 3 :dir [0 -1]})
               (map->Node {:row row :col (inc col) :block 3 :dir [0 1]})
               (map->Node {:row (dec row) :col col :block (dec block) :dir [-1 0]})]))))

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
    (visualize
     lines
     (g/shortest-paths
      (zip/zipper
       (constantly true)
       (partial children max-row max-col)
       (constantly nil)
       (map->Node {:row 0 :col 0 :block 4}))
      (fn edges [parent {:keys [row col] :as child}]
        [(->Edge parent child (- (byte (get-in lines [row col])) 48))])
      (fn [a b]
        (letfn [(agg [n {:keys [heat-loss]}]
                  (+ n heat-loss))]
          (cond
            (< (reduce agg 0 a) (reduce agg 0 b)) -1
            :else 1)))
      [(dec max-row) (dec max-col)]))))
