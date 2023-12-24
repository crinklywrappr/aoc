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

(defn edges [graph]
  (let [f (:graph/edges (meta graph))
        p (zip/node graph)]
    (reduce
     (fn [a b]
       (concat a (f p b)))
     [] (zip/children graph))))

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

(defn min-path [old-path new-path]
  (if (neg? (compare-paths new-path old-path))
    new-path old-path))

(defn analyze-paths [visited active distances]
  (reduce-kv
   (fn analyze-paths' [[new-active new-distances :as acc] parent-id parent]
     (loop [active? false new-distances' new-distances
            [edge & edges] (edges parent)]
       (if (nil? edge)
         (if active?
           [(assoc new-active parent-id parent) new-distances']
           [new-active new-distances'])
         (let [child-id (id (child edge))]
           (if (contains? visited child-id)
             (recur active? new-distances' edges)
             (let [parent-path (get visited parent-id)]
               (if-let [old-path (get new-distances' child-id)]
                 (let [new-path (make-path parent-path (graph-at-node old-path) edge)]
                   (recur true (update new-distances' child-id min-path new-path) edges))
                 (let [new-path (make-path parent-path edge)]
                   (recur true (assoc new-distances' child-id new-path) edges)))))))))
   [{} distances] active))

(defn dijkstra
  "Performs Dijkstra's shortest path."
  [graph]
  (loop [visited {(identify graph) (make-path graph)}
         active {(identify graph) graph}
         distances (pm/priority-map-keyfn-by identity compare-paths)]
    (let [[active' distances'] (analyze-paths visited active distances)]
      (if (and (seq active') (seq distances'))
        (let [[next-node next-path] (peek distances')]
          (recur (assoc visited next-node next-path)
                 (assoc active' next-node (graph-at-node next-path))
                 (pop distances')))
        (merge visited distances)))))
