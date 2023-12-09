(ns crinklywrappr.aoc.2016.day1
  (:require [clojure.java.io :as io]))

(defn parse [instr]
  [(first instr) (parse-long (subs instr 1))])

(def input (->> (io/resource "2016/day1.txt")
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
