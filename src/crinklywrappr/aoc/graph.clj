(ns crinklywrappr.aoc.graph
  (:require [clojure.zip :as zip]))

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

(defn exclude-edge? [visited]
  (fn [edge]
    (contains? visited (id (child edge)))))

(defn exclude-path? [visited]
  (fn [[k _]]
    (contains? visited k)))

(defn shortest-distance [path-cmp visited distances]
  (->> distances
       (remove (exclude-path? visited))
       (sort-by (comp :edges val) path-cmp)
       first))

(defn analyze-edges [path-cmp visited active distances edges]
  (reduce
   (fn [[parents [shortest-child {shortest-path :edges} :as shortest] new-distances] edge]
     (let [parent (id (parent edge))
           child (child edge)
           child-id (id child)
           old-child-path (:edges (get new-distances child-id))
           new-child-path (conj (:edges (get distances parent)) edge)]
       [(conj parents parent)
        (if (or (nil? shortest) (neg? (path-cmp new-child-path shortest-path)))
          [child-id new-child-path] shortest)
        (if (or (not (contains? new-distances child-id))
                (neg? (path-cmp new-child-path old-child-path)))
          (assoc new-distances child-id {:zipper (find-child (get active parent) child)
                                         :edges new-child-path})
          new-distances)]))
   [#{} (shortest-distance path-cmp visited distances) distances] edges))

(defn dijkstra
  "Performs Dijkstra's shortest path.
  - `graph` is a zipper that uses `Node` objects.
  - `edge-fn` is a function that takes two nodes and returns a sequence of `Edge` objects.
              If there are no edges, return an empty seq or `nil`.
  - `path-cmp` is a comparator function for sequences of `Edge` objects."
  [graph edge-fn path-cmp]
  (loop [visited #{(identify graph)}
         active {(identify graph) graph}
         distances {(identify graph) {:zipper graph :edges []}}]
    (if-let [edges (seq (remove (exclude-edge? visited) (mapcat (partial edges edge-fn) (vals active))))]
      (let [[parents [shortest-child] new-distances] (analyze-edges path-cmp visited active distances edges)
            new-zipper (get-in new-distances [shortest-child :zipper])]
        (recur (conj visited shortest-child)
               (-> active
                   (select-keys parents)
                   (assoc shortest-child new-zipper))
               new-distances))
      distances)))
