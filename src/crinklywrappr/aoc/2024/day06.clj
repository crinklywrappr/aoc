(ns crinklywrappr.aoc.2024.day06
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2024/day06.txt"))
(def sz 129)

(defn parse-line [line]
  (util/re-pos #"#|\^" line))

(defn turn-right [dir]
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up))

(defn scan-back [obstacles n]
  (loop [prev 0 [x & xs] obstacles]
    (if (or (nil? x) (> x n))
      prev
      (recur (inc x) xs))))

(defn scan-forward [obstacles n]
  (loop [prev sz [x & xs] (reverse obstacles)]
    (if (or (nil? x) (< x n))
      prev
      (recur (dec x) xs))))

(defn scan-direction [by-row by-col [row col :as pos] dir]
  [pos
   (case dir
     :left [row (scan-back (get by-row row) col)]
     :up [(scan-back (get by-col col) row) col]
     :right [row (scan-forward (get by-row row) col)]
     :down [(scan-forward (get by-col col) row) col])])

(defn patrol [[by-row by-col pos]]
  (iteration
   (fn move [[_ pos dir]]
     (let [[_ [r c] :as path] (scan-direction by-row by-col pos dir)]
       [path [r c] (turn-right dir)]))
   :vf first
   :kf (fn end? [[_ [r c] & _ :as state]]
         (when (not (or (= r 0) (= r sz) (= c 0) (= c sz)))
           state))
   :initk [nil pos :up]))

(defn add-row [row-no [by-row by-col pos] [k v]]
  (if (= v "#")
    [(update by-row row-no (fnil conj (sorted-set)) k)
     (update by-col k (fnil conj (sorted-set)) row-no)
     pos]
    [by-row by-col [row-no k]]))

(defn accrue-state [[state row-no] obstacles]
  [(reduce (partial add-row row-no) state obstacles) (inc row-no)])

(defn build-state []
  (with-open [rdr (io/reader file)]
    (-> (map parse-line)
        (transduce (completing accrue-state)
                   [[{} {} nil] 0] (line-seq rdr))
        first)))

(defn visit [a [[row col] [row' col']]]
  (if (== row row')
    (into a (mapv #(vector row %) (range (min col col') (inc (max col col')))))
    (into a (mapv #(vector % col) (range (min row row') (inc (max row row')))))))

(defn part1 []
  (->> (build-state)
       patrol
       (reduce visit #{})
       count))

(defn sabotaged-path? [loc [obstacle-row obstacle-col]]
  (or (= loc [(dec obstacle-row) obstacle-col])
      (= loc [(inc obstacle-row) obstacle-col])
      (= loc [obstacle-row (dec obstacle-col)])
      (= loc [obstacle-row (inc obstacle-col)])))

(defn infinite-loop? [[by-row by-col pos] sabotage]
  (loop [[_ [r c] :as path] (scan-direction by-row by-col pos :up)
         pos pos dir :up record? false check-paths #{}]
    (cond
      (contains? check-paths path) true
      (or (= r 0) (= r sz) (= c 0) (= c sz)) false
      :else (let [dir' (turn-right dir)
                  path' (scan-direction by-row by-col [r c] dir')]
              (if (sabotaged-path? [r c] sabotage)
                (recur path' [r c] dir' true (conj check-paths path))
                (recur path' [r c] dir' record?
                       (if record? (conj check-paths path) check-paths)))))))

(defn add-sabotage [state [sabotage-row sabotage-col :as sabotage]]
  (-> state
      (update-in [0 sabotage-row] (fnil conj (sorted-set)) sabotage-col)
      (update-in [1 sabotage-col] (fnil conj (sorted-set)) sabotage-row)))

(defn sabotage-works? [state total sabotage]
  (if (infinite-loop? (add-sabotage state sabotage) sabotage)
    (inc total)
    total))

(defn part2 []
  (let [[_ _ pos :as state] (build-state)
        visited (reduce visit #{} (patrol state))]
    (reduce (partial sabotage-works? state) 0 (disj visited pos))))
