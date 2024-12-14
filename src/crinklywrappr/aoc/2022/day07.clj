(ns crinklywrappr.aoc.2022.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]
            [clojure.zip :as z]))

(def file (io/resource "2022/day07.txt"))

(defn dir? [node]
  (contains? (meta node) :children))

(defn filesystem []
  (z/zipper
   dir? (comp seq :children meta)
   (fn make-node [node children]
     (with-meta node {:children children}))
   (with-meta {:filename "/"} {:children []})))

(defn make-dir-node [filename]
  (with-meta {:filename filename} {:children []}))

(defn make-file-node [filename size]
  (with-meta {:filename filename} {:size size}))

(defn root [filesystem]
  (if-let [parent (z/up filesystem)]
    (recur parent)
    filesystem))

(defn cd [filesystem file]
  (loop [filesystem (z/down filesystem)]
    (let [{:keys [filename]} (z/node filesystem)]
      (if (= filename file)
        filesystem
        (recur (z/right filesystem))))))

(defn populate-filesystem [filesystem line]
  (cond
    (= line "$ ls") filesystem
    (= line "$ cd /") (root filesystem)
    (= line "$ cd ..") (z/up filesystem)
    (sg/starts-with? line "$ cd") (cd filesystem (.substring line 5))
    (sg/starts-with? line "dir") (->> (.substring line 4)
                                      make-dir-node
                                      (z/insert-child filesystem))
    :else (let [[size filename] (sg/split line #" ")]
            (->> (parse-long size)
                 (make-file-node filename)
                 (z/insert-child filesystem)))))

(defn update-totals [directories filesystem size]
  (first
   (reduce
    (fn [[directories' path] {:keys [filename]}]
      (let [path' (conj path filename)]
        [(update directories' path' (fnil + 0) size) path']))
    [directories []] (reverse (z/path filesystem)))))

(defn df [filesystem]
  (loop [directories {} filesystem (root filesystem)]
    (if (z/end? filesystem)
      directories
      (let [node (z/node filesystem)]
        (if (dir? node)
          (recur directories (z/next filesystem))
          (recur (update-totals directories filesystem (:size (meta node)))
                 (z/next filesystem)))))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (reduce-kv
     (fn [a _ v] (if (< v 100000) (+ a v) a))
     0 (df (reduce populate-filesystem (filesystem) (line-seq rdr))))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (let [size-map (df (reduce populate-filesystem (filesystem) (line-seq rdr)))
          target (- 30000000 (- 70000000 (get size-map ["/"])))]
      (reduce-kv
       (fn [a _ v] (if (and (>= v target) (< v a)) v a))
       30000000 size-map))))
