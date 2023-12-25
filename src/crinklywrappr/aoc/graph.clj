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

(defprotocol IPath
  (graph-at-node [_] "Returns a zipper representing the graph at this node."))

(defn graph
  "Creates a zipper with the following additional metadata
  - `children` is a function that takes an `INode` value and returns a sequence of adjacent `INode` objects
               If there are no adjacent nodes, return an empty seq or `nil`.
  - `edges` is a function that takes two nodes and returns a sequence of `IEdge` objects.
            If there are no edges, return an empty seq or `nil`.
  - `make-path` function with two arities:
                single-arity function which takes a graph and returns an `IPath` object.
                three-arity function which also returns a new path.  The params are:
                - parent path
                - graph at the new (child) node
                - and the edge used to reach it
  - `path-comparator` is a comparator function for sequences of `IPath` objects.
  - `root` is a value that satisfies the `INode` protocol"
  [children edges make-path path-comparator root]
  (vary-meta
   (zip/zipper (constantly true) children (constantly nil) root)
   assoc :graph/edges edges :graph/make-path make-path :graph/path-comparator path-comparator))

(defn edges
  "child is a node, not an id"
  [graph child]
  ((:graph/edges (meta graph)) (zip/node graph) child))

(defn compare-paths [path1 path2]
  ((:graph/path-comparator (meta (graph-at-node path1))) path1 path2))

(defn identify [graph]
  (id (zip/node graph)))

(defn find-child [graph search]
  (loop [nd (zip/down graph)]
    (if (= (zip/node nd) search)
      nd (recur (zip/right nd)))))

(defn make-path
  ([graph]
   ((:graph/make-path (meta graph)) graph))
  ([parent-path edge]
   (let [g (graph-at-node parent-path)]
     (make-path parent-path (find-child g (child edge)) edge)))
  ([parent-path graph edge]
   ((:graph/make-path (meta graph)) parent-path graph edge)))

(defn min-path [old-path parent-path edge]
  (if (nil? old-path)
    (make-path parent-path edge)
    (let [new-path (make-path parent-path (graph-at-node old-path) edge)]
      (if (neg? (compare-paths new-path old-path))
        new-path old-path))))

(defn analyze-edges [distances parent-graph parent-path child-id child-node]
  (reduce
   (fn analyze-edges' [distances' edge]
     (update distances' child-id min-path parent-path edge))
   distances (edges parent-graph child-node)))

(defn analyze-children [visited parent-graph parent-path distances]
  (reduce
   (fn analyze-children' [distances' child-node]
     (let [child-id (id child-node)]
       (if (contains? visited child-id)
         distances'
         (analyze-edges distances' parent-graph parent-path child-id child-node))))
   distances (zip/children parent-graph)))

(defn dijkstra
  "Performs Dijkstra's shortest path."
  [graph]
  (loop [visited {(identify graph) (make-path graph)}
         active-graph graph active-path (make-path graph)
         distances (pm/priority-map-keyfn-by identity compare-paths)]
    (let [distances' (analyze-children visited active-graph active-path distances)]
      (if (seq distances')
        (let [[next-id next-path] (peek distances')]
          (recur (assoc visited next-id next-path)
                 (graph-at-node next-path) next-path
                 (pop distances')))
        visited))))
