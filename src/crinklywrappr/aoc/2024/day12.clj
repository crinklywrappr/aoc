(ns crinklywrappr.aoc.2024.day12
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]))

(def file (io/resource "2024/day12.txt"))

(def farm-source
  (with-open [rdr (io/reader file)]
    (mapv vec (line-seq rdr))))

(def height (count farm-source))
(def width (count (first farm-source)))

(defn plant-at [farm row col]
  (get-in farm [row col]))

(defn mark-plot! [farm row col]
  (swap! farm assoc-in [row col] \.))

(defn plot-marked? [farm row col]
  (= (plant-at @farm row col) \.))

(defn identify-borders [row col]
  (let [plant (plant-at farm-source row col)]
    (cond-> #{}
      (not= plant (plant-at farm-source (dec row) col)) (conj :u)
      (not= plant (plant-at farm-source row (inc col))) (conj :r)
      (not= plant (plant-at farm-source (inc row) col)) (conj :d)
      (not= plant (plant-at farm-source row (dec col))) (conj :l))))

(defn adjacent-plots [farm row col]
  [[(dec row) col (plant-at @farm (dec row) col) (identify-borders (dec row) col)]
   [row (inc col) (plant-at @farm row (inc col)) (identify-borders row (inc col))]
   [(inc row) col (plant-at @farm (inc row) col) (identify-borders (inc row) col)]
   [row (dec col) (plant-at @farm row (dec col)) (identify-borders row (dec col))]])

(defn find-children [farm row col plant]
  (->> (adjacent-plots farm row col)
       (keep
        (fn [[_ _ p _ :as node]]
          (when (= p plant) node)))
       seq))

(defn region-finder [farm row col]
  (mark-plot! farm row col)
  (z/zipper
   (constantly true)
   (fn children [[row col plant _]]
     (let [xs (find-children farm row col plant)]
       (doseq [[r c _ _] xs]
         (mark-plot! farm r c))
       xs))
   (constantly nil)
   [row col (plant-at farm-source row col) (identify-borders row col)]))

(defn find-region [g]
  (loop [area 0 perimeter 0 g g]
    (if (z/end? g)
      (* area perimeter)
      (let [[row col plant borders :as node] (z/node g)]
        (recur (inc area) (+ perimeter (count borders)) (z/next g))))))

(defn part1 []
  (let [farm (atom farm-source)]
    (->>
     (for [row (range height)
           col (range width)
           :when (not (plot-marked? farm row col))]
       (find-region (region-finder farm row col)))
     (apply +))))

(defn count-sides' [partial-coords]
  (first
   (reduce
    (fn [[total prev-coord] coord]
      (if (== prev-coord (dec coord))
        [total coord]
        [(inc total) coord]))
    [1 (first partial-coords)] (rest partial-coords))))

(defn count-sides [dir grouping-fn keyfn borders]
  (->> borders
       (filter (fn [[_ _ _ b]] (contains? b dir)))
       (group-by grouping-fn) vals
       (map (comp count-sides'
               (partial sort <)
               (partial map keyfn)))
       (apply +)))

(defn fence-price [g]
  (loop [area 0 borders [] g g]
    (if (z/end? g)
      (* area (+ (count-sides :u first second borders)
                 (count-sides :r second first borders)
                 (count-sides :d first second borders)
                 (count-sides :l second first borders)))
      (let [[row col plant borders' :as node] (z/node g)]
        (if (seq borders')
          (recur (inc area) (conj borders node) (z/next g))
          (recur (inc area) borders (z/next g)))))))

(defn part2 []
  (let [farm (atom farm-source)]
    (->>
     (for [row (range height)
           col (range width)
           :when (not (plot-marked? farm row col))]
       (fence-price (region-finder farm row col)))
     (apply +))))
