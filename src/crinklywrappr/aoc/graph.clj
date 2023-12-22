(ns crinklywrappr.aoc.graph
  (:require [clojure.zip :as zip]
            [clojure.data.priority-map :as pm]))

;; intentionally anemic protocols

(defprotocol INode
  (id [_] "A unique id or name for this node"))

(defprotocol IEdge
  "Does not express directionality"
  (parent [_] "The node that was passed to `edges`")
  (child [_] "A \"child\" node."))

(defn graph
  "Creates a zipper with the following additional metadata
  - `children` is a function that takes an `INode` value and returns a sequence of adjacent `INode` objects
               If there are no adjacent nodes, return an empty seq or `nil`.
  - `edges` is a function that takes two nodes and returns a sequence of `IEdge` objects.
            If there are no edges, return an empty seq or `nil`.
  - `path-comparator` is a comparator function for sequences of `IEdge` objects.
  - `root` is a value that satisfies the `INode` protocol"
  [children edges path-comparator root]
  (vary-meta
   (zip/zipper (constantly true) children (constantly nil) root)
   assoc :graph/edges edges :graph/path-comparator path-comparator))

(defn edges [graph]
  (mapcat
   (partial (:graph/edges (meta graph))
      (zip/node graph))
   (zip/children graph)))

(defn identify [graph]
  (id (zip/node graph)))

(defn find-child [graph search]
  (loop [nd (zip/down graph)]
    (if (= (zip/node nd) search)
      nd (recur (zip/right nd)))))

(defn min-path [{old-path :edges :as old} new-path path-cmp parent child]
  (if (neg? (path-cmp new-path old-path))
    {:zipper (find-child parent child) :edges new-path}
    old))

(defn analyze-paths [path-cmp visited active distances]
  (reduce-kv
   (fn [[new-active new-distances :as acc] parent-id parent]
     (loop [active? false new-distances' new-distances
            [edge & edges] (edges parent)]
       (if (nil? edge)
         (if active?
           [(assoc new-active parent-id parent) new-distances']
           [new-active new-distances'])
         (let [child (child edge) child-id (id child)]
           (if (contains? visited child-id)
             (recur active? new-distances' edges)
             (let [new-path (conj (:edges (get visited parent-id)) edge)]
               (if (contains? new-distances' child-id)
                 (recur true (update new-distances' child-id min-path new-path path-cmp parent child) edges)
                 (recur true (assoc new-distances' child-id {:zipper (find-child parent child) :edges new-path}) edges))))))))
   [{} distances] active))

(defn dijkstra
  "Performs Dijkstra's shortest path."
  [graph]
  (let [path-cmp (:graph/path-comparator (meta graph))]
    (loop [visited (transient {(identify graph) {:zipper graph :edges []}})
           active {(identify graph) graph}
           distances (pm/priority-map-keyfn-by :edges path-cmp)]
      (let [[active' distances'] (analyze-paths path-cmp visited active distances)]
        (if (and (seq active') (seq distances'))
          (let [[next-node next-path] (peek distances')]
            (recur (assoc! visited next-node next-path)
                   (assoc active' next-node (:zipper next-path))
                   (dissoc distances' next-node)))
          (merge (persistent! visited) distances))))))
