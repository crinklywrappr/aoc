(ns crinklywrappr.aoc.2016.day01
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]
            [crinklywrappr.aoc.geom :as geom])
  (:import [java.io BufferedReader]))

(def file (io/resource "2016/day01.txt"))

(defn parse [instr]
  [[(first instr) (parse-long (subs instr 1))]])

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

(defn read-directions [^BufferedReader rdr]
  (let [comma-or-end? (fn [c] (or (= c \,) (= c \newline)))
        next-fn (fn [xs] (apply str (take-while (complement comma-or-end?) xs)))
        advance? (fn [chunk _] (seq chunk))
        rest-fn (fn [chunk xs] (drop (+ (count chunk) 2) xs))
        f (util/chunk-seq next-fn advance? rest-fn parse)]
    (f (util/char-seq rdr))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (->> (read-directions rdr)
         (reduce visit [[0 0] :north])
         first (mapv abs) (apply +))))

(defn visited? [old-coords new-coords prior-path]
  (when (geom/intersects? (vec prior-path) [old-coords new-coords])
    (geom/intersection (vec prior-path) [old-coords new-coords])))

(defn track [[visited & [old-coords :as state]] directions]
  (let [[[x y :as coords] :as new-state] (visit state directions)]
    (if-let [[x y] (some (partial visited? old-coords coords) (butlast (partition 2 1 visited)))]
      (reduced (+ (abs x) (abs y)))
      (vec (cons (conj visited coords) new-state)))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (reduce track [[[0 0]] [0 0] :north] (read-directions rdr))))
