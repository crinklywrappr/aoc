(ns crinklywrappr.aoc.2022.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2022/day05.txt"))

(defn parse-line [line]
  (cond
    (sg/starts-with? line "m")
    (mapv parse-long (re-seq #"\d+" line))

    (sg/includes? line "[")
    (->> (util/re-pos #"[A-Z]" line)
         (mapv (fn [[i x]] [i (first x)]))
         (into {}))

    (sg/starts-with? line " ")
    (->> (util/re-pos #"\d+" line)
         (mapv (fn [[i x]] [i (list)]))
         (into (sorted-map)))))

(defn stack-crate [state [i crate]]
  (update state i conj crate))

(defn build-state [state crates]
  (reduce stack-crate state crates))

(defn transform-state [state]
  (->> (butlast state) reverse
       (reduce build-state (last state))
       (mapv val)))

(defn move-crate [state from to]
  (let [crate (peek (get state from))]
    (-> state
        (update from pop)
        (stack-crate [to crate]))))

(defn move-crates [state [qty from to]]
  (if (zero? qty)
    state
    (recur (move-crate state (dec from) (dec to))
           [(dec qty) from to])))

(defn build-and-stack [move-fn state instructions]
  (cond
    (map? instructions ) (conj state instructions)
    (vector? instructions) (move-fn state instructions)
    :else (transform-state state)))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map parse-line)
                    (completing (partial build-and-stack move-crates))
                    [])
         (reduce #(str %1 (peek %2)) ""))))

(defn bulk-move-crates [state [qty from to]]
  (let [crates (take qty (get state (dec from)))]
    (-> state
        (update (dec from) (partial drop qty))
        (update (dec to) (partial concat crates)))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (transduce (map parse-line)
                    (completing (partial build-and-stack bulk-move-crates))
                    [])
         (reduce #(str %1 (first %2)) ""))))
