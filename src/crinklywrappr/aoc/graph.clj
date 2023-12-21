(ns crinklywrappr.aoc.graph
  (:require [clojure.zip :as zip]
            [clojure.data.priority-map :as pm]))

;; intentionally anemic protocols

(defprotocol Node
  (id [_] "A unique id or name for this node"))

(defprotocol Edge
  "Does not express directionality"
  (parent [_] "The node that was passed to `edges`")
  (child [_] "A \"child\" node."))

(defn edges [edge-fn graph]
  (mapcat (partial edge-fn (zip/node graph))
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

(defn analyze-paths [edge-fn path-cmp visited active distances]
  (reduce-kv
   (fn [[new-visited new-active new-distances :as acc] parent-id parent]
     (loop [active? false new-distances' new-distances
            [edge & edges] (edges edge-fn parent)]
       (if (nil? edge)
         (if active?
           [new-visited (assoc new-active parent-id parent) new-distances']
           [new-visited new-active new-distances'])
         (let [child (child edge) child-id (id child)]
           (if (contains? visited child-id)
             (recur active? new-distances' edges)
             (let [new-path (conj (:edges (get new-visited parent-id)) edge)]
               (if (contains? new-distances' child-id)
                 (recur true (update new-distances' child-id min-path new-path path-cmp parent child) edges)
                 (recur true (assoc new-distances' child-id {:zipper (find-child parent child) :edges new-path}) edges))))))))
   [visited {} distances] active))

(defn dijkstra
  "Performs Dijkstra's shortest path.
  - `graph` is a zipper that uses `Node` objects.
  - `edge-fn` is a function that takes two nodes and returns a sequence of `Edge` objects.
              If there are no edges, return an empty seq or `nil`.
  - `path-cmp` is a comparator function for sequences of `Edge` objects."
  [graph edge-fn path-cmp]
  (loop [visited {(identify graph) {:zipper graph :edges []}}
         active {(identify graph) graph}
         distances (pm/priority-map-keyfn-by :edges path-cmp)]
    (let [[visited' active' distances'] (analyze-paths edge-fn path-cmp visited active distances)]
      (if (and (seq active') (seq distances'))
        (let [[next-node next-path] (peek distances')]
          (recur (assoc visited' next-node next-path)
                 (assoc active' next-node (:zipper next-path))
                 (dissoc distances' next-node)))
        (merge visited distances)))))
