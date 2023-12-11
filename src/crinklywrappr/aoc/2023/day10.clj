(ns crinklywrappr.aoc.2023.day10
  (:require [clojure.string :as sg]
            [clojure.java.io :as io]))

(def input
  (as-> "2023/day10.txt" $
    (io/resource $)
    (slurp $)
    (.split $ "\n")
    (vec $)))

(defn pipe-at [[row col]]
  (-> input
      (get row)
      (get col)))

(defn find-animal []
  (some
   (fn [[row s]]
     (when-let [col (sg/index-of s \S)]
       [row col]))
   (partition 2 (interleave (range) input))))

(defn init-state []
  (let [[row col] (find-animal)]
    (cond
      (contains? #{\┐ \─ \┘} (pipe-at [row (inc col)])) [[row col] 0 :east \S]
      (contains? #{\│ \┘ \└} (pipe-at [(inc row) col])) [[row col] 0 :south \S]
      (contains? #{\└ \─ \┌} (pipe-at [row (dec col)])) [[row col] 0 :west \S]
      (contains? #{\│ \┐ \┌} (pipe-at [(dec row) col])) [[row col] 0 :north \S])))

(defn next-dir [dir pipe]
  (case [dir pipe]
    [:east \┐] :south
    [:east \┘] :north

    [:south \└] :east
    [:south \┘] :west

    [:west \┌] :south
    [:west \└] :north

    [:north \┌] :east
    [:north \┐] :west))

(defn next-state [row col turns dir]
  (let [pos
        (case dir
          :east [row (inc col)]
          :south [(inc row) col]
          :west [row (dec col)]
          :north [(dec row) col])
        pipe (pipe-at pos)]
    (cond
      (= pipe \S) (/ (inc turns) 2)
      (or (= pipe \│) (= pipe \─)) [pos (inc turns) dir pipe]
      :else [pos (inc turns) (next-dir dir pipe) pipe])))

(defn part1 []
  (loop [[[row col] turns dir _] (init-state)]
    (let [state (next-state row col turns dir)]
      (if (vector? state) (recur state) state))))

(defn update-bounds [row-bounds pipe row col]
  (case pipe
    \─ row-bounds
    \│ (update row-bounds row conj col col)
    (update row-bounds row conj col)))

(defn vertical-pipe? [row col1 col2]
  (case [(pipe-at [row col1]) (pipe-at [row col2])]
    [\┌ \┘] true
    [\└ \┐] true
    [\S \┐] true ;; handle your special S case here
    (== col1 col2)))

(defn count-enclosed-tiles [[row bounds]]
  (first
   (reduce
    (fn step [[tiles ray last-pipe] [col1 col2]]
      [(if (odd? ray) (+ tiles (- col1 last-pipe 1)) tiles)
       (if (vertical-pipe? row col1 col2) (inc ray) (+ ray 2))
       col2])
    [0 0 0] (->> bounds sort (partition 2)))))

;; requires special case when S is in the
;; middle of two horizontal pipes, but my
;; input file didn't require it
(defn part2 []
  (->>
   (loop [row-bounds {} [[row col] turns dir pipe] (init-state)]
     (let [state (next-state row col turns dir)]
       (if (vector? state)
         (recur (update-bounds row-bounds pipe row col) state)
         (update-bounds row-bounds pipe row col))))
   (mapv count-enclosed-tiles) (apply +)))
