(ns crinklywrappr.aoc.2016.day01
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.geom :as geom]))

(defn parse [instr]
  [(first instr) (parse-long (subs instr 1))])

(def input (->> (io/resource "2016/day01.txt")
                (slurp) (re-seq #"[RL]\d+")
                (mapv parse)))

(defn visit [[[x y] facing] [dir steps]]
  (case [facing dir]
    [:north \R] [[(+ x steps) y] :east]
    [:north \L] [[(- x steps) y] :west]
    [:east \R] [[x (+ y steps)] :south]
    [:east \L] [[x (- y steps)] :north]
    [:south \R] [[(- x steps) y] :west]
    [:south \L] [[(+ x steps) y] :east]
    [:west \R] [[x (- y steps)] :north]
    [:west \L] [[x (+ y steps)] :south]))

(defn part1 []
  (->> input
       (reduce visit [[0 0] :north])
       first (mapv abs) (apply +)))

(defn visited? [old-coords new-coords prior-path]
  (when (geom/intersects? (vec prior-path) [old-coords new-coords])
    (geom/intersection (vec prior-path) [old-coords new-coords])))

(defn track [[visited & [old-coords :as state]] directions]
  (let [[[x y :as coords] :as new-state] (visit state directions)]
    (if-let [[x y] (some (partial visited? old-coords coords) (butlast (partition 2 1 visited)))]
      (reduced (+ (abs x) (abs y)))
      (vec (cons (conj visited coords) new-state)))))

(defn part2 []
  (reduce track [[[0 0]] [0 0] :north] input))
