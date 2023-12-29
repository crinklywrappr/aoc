(ns crinklywrappr.aoc.2022.day16
  (:require [clojure.zip :as zip]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.graph :as g]))

(def file (io/resource "2022/day16.txt"))

(defn parse-line [line]
  (let [[valve flow-rate & connections] (re-seq #"[A-Z][A-Z]|\d+" line)]
    [valve {:flow-rate (parse-long flow-rate) :pipes (vec connections)}]))

(def gsource
  (with-open [rdr (io/reader file)]
    (reduce
     (fn [a b] (conj a (parse-line b)))
     {} (line-seq rdr))))

(defrecord Node [valve t open total flow-rate]
  g/INode
  (id [_] valve))

(defrecord Edge [from to]
  g/IEdge
  (parent [_] from)
  (child [_] to))

(defrecord Path [graph score t]
  g/IPath
  (graph-at-node [_] graph))

(def pipes
  (g/graph
   (fn children [{:keys [valve t open total]}]
     (if (== t 1)
       []
       (mapv
        (fn [child]
          (->Node child (dec t) open total
                  (if (contains? open child)
                    0 (-> gsource (get child) :flow-rate))))
        (-> gsource (get valve) :pipes))))
   (fn edges [from-valve to-valve]
     [(->Edge from-valve to-valve)])
   (fn make-path
     ([graph] (->Path graph 0 (:t (zip/node graph))))
     ([{:keys [score]} graph _]
      (let [{:keys [t flow-rate]} (zip/node graph)]
        (if (and (> t 2) (pos? flow-rate))
          (->Path graph (* flow-rate (- t 2)) (dec t))
          (->Path graph 0 t)))))
   (fn path-comparator [{a :t} {b :t}]
     (cond
       (> a b) -1
       (< a b) 1
       :else 0))
   (->Node "AA" 31 #{} 0 0)))

(defn open-valve [node valve score t]
  (-> node
      (assoc :t t)
      (update :total + score)
      (update :open conj valve)))

(defn contest [a b]
  (if (or (nil? a) (< (:total (zip/node a)) (:total (zip/node b))))
    b a))

(defn act [[winner unfinished] pgraph]
  (if-let [paths (seq (filter (comp pos? :score val) (g/dijkstra pgraph)))]
    [winner
     (reduce-kv
      (fn act' [unfinished valve {:keys [graph score t]}]
        (conj unfinished (zip/edit graph open-valve valve score t)))
      unfinished paths)]
    [(contest winner pgraph) unfinished]))

(defn part1 []
  (loop [winner nil unfinished [pipes]]
    (if (seq unfinished)
      (let [[finished' unfinished'] (reduce act [winner []] unfinished)]
        (recur finished' unfinished'))
      (:total (zip/node winner)))))
