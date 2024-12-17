(ns crinklywrappr.aoc.2024.day15
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]
            [clojure.string :as sg]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2024/day15.txt"))

(defn find-bot [line]
  (util/re-pos #"@" line))

(defn build-map [[grid loc row] line]
  (let [loc' (find-bot line)]
    (if-let [col (ffirst loc')]
      [(conj grid (replace {\@ \.} (vec line))) [row col] (inc row)]
      [(conj grid (vec line)) loc (inc row)])))

(defn next-coord [[row col] [dr dc]]
  [(+ row dr) (+ col dc)])

(defn inspect [grid [row col]]
  (get-in grid [row col]))

(defn scan [grid loc dir]
  (loop [ret [] coord (next-coord loc dir)]
    (case (inspect grid coord)
      \O (recur (if (seq ret) ret [coord]) (next-coord coord dir))
      \. (conj ret coord)
      \# [])))

(defn get-dir [command]
  (case command
    \< [0 -1]
    \> [0 1]
    \^ [-1 0]
    \v [1 0]))

(defn execute [[grid loc] command]
  (let [dir (get-dir command)
        flips (scan grid loc dir)]
    (case (count flips)
      0 [grid loc]
      1 [grid (next-coord loc dir)]
      2 (let [[old new] flips]
          [(-> grid
               (assoc-in old \.)
               (assoc-in new \O))
           old]))))

(defn build-and-execute [build-fn execute-fn state input]
  (cond
    (sg/starts-with? input "#") (build-fn state input)
    :else (reduce execute-fn state input)))

(defn gps-coordinates [[grid _]]
  (->> (for [row (range (count grid))
             col (range (count (first grid)))
             :let [element (inspect grid [row col])]
             :when (or (= \[ element) (= \O element))]
         (+ (* 100 row) col))
       (apply +)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (gps-coordinates
     (reduce (partial build-and-execute build-map execute)
             [[] nil 0] (line-seq rdr)))))

(defn expand-row [line loc row]
  (reduce
   (fn [[line loc i] b]
     (case b
       \# [(conj line \# \#) loc (+ i 2)]
       \O [(conj line \[ \]) loc (+ i 2)]
       \. [(conj line \. \.) loc (+ i 2)]
       \@ [(conj line \. \.) [row i] (+ i 2)]))
   [[] loc 0] line))

(defn build-larger-map [[grid loc row] line]
  (let [[expanded-row loc _] (expand-row line loc row)]
    [(conj grid expanded-row) loc (inc row)]))

(defn scan-horizontal [grid loc dir]
  (loop [ret (list) coord (next-coord loc dir)]
    (case (inspect grid coord)
      \[ (recur (conj ret coord)
                (next-coord coord dir))
      \] (recur ret (next-coord coord dir))
      \. [true ret]
      \# [false []])))

(defn vertical-scanner [grid loc dir]
  (letfn [(snap [[[row col] element :as node]]
            (if (= element \])
              [[row (dec col)] \[ ]
              node))]
    (z/zipper
     (fn branch? [[_ element]] (= element \[))
     (fn children [[coord element]]
       (let [c1 (next-coord coord dir)
             e1 (inspect grid c1)
             c2 (-> coord
                    (next-coord [0 1])
                    (next-coord dir))
             e2 (inspect grid c2)]
         [(snap [c1 e1]) [c2 e2]]))
     (constantly nil)
     (let [start (next-coord loc dir)]
       (snap [start (inspect grid start)])))))

(defn scan-vertical [grid loc dir]
  (loop [obstacles [] scanner (vertical-scanner grid loc dir)]
    (if (z/end? scanner)
      [true
       (if (= dir [-1 0])
         (sort-by first < obstacles)
         (sort-by first > obstacles))]
      (let [[coord element] (z/node scanner)]
        (cond
          (= element \#) [false []]
          (= element \[) (recur (conj obstacles coord) (z/next scanner))
          :else (recur obstacles (z/next scanner)))))))

(defn scan-more [grid loc [dr _ :as dir]]
  (if (zero? dr)
    (scan-horizontal grid loc dir)
    (scan-vertical grid loc dir)))

(defn move-obstacles [grid moves dir]
  (reduce
   (fn [grid coord]
     (-> grid
         (assoc-in coord \.)
         (assoc-in (next-coord coord [0 1]) \.)
         (assoc-in (next-coord coord dir) \[)
         (assoc-in (-> coord
                       (next-coord [0 1])
                       (next-coord dir)) \])))
   grid moves))

(defn execute-more [[grid loc] command]
  (let [dir (get-dir command)
        [move? moves] (scan-more grid loc dir)]
    (cond
      (not move?) [grid loc]
      (and move? (empty? moves)) [grid (next-coord loc dir)]
      :else [(move-obstacles grid moves dir) (next-coord loc dir)])))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (gps-coordinates
     (reduce (partial build-and-execute build-larger-map execute-more)
             [[] nil 0] (line-seq rdr)))))

