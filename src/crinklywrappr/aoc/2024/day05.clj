(ns crinklywrappr.aoc.2024.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]))

(def file (io/resource "2024/day05.txt"))

(defn parse-line [line]
  (when (seq line)
    [(sg/includes? line "|")
     (mapv parse-long (re-seq #"\d+" line))]))

(defn midpoint [pages]
  (pages (long (/ (count pages) 2))))

(defn invalid-prior? [after current prior]
  (contains? (get after prior) current))

(defn invalid-post? [before current post]
  (contains? (get before post) current))

;; before: a => [b1, b2, ...] => a is BEFORE b1, b2
;; after:  a => [b1, b2, ...] => a is  AFTER b1, b2
(defn ordered? [before after pages]
  (loop [prior [] current (first pages) post (rest pages)]
    (cond
      (nil? current) true
      ;; no rule where prior after current
      ;; and no rule where post before current
      (or (some (partial invalid-prior? after current) prior)
          (some (partial invalid-post? before current) post)) false
      :else (recur (conj prior current) (first post) (rest post)))))

(defn build-and-apply-rules
  [[total before after] [rule? pages]]
  (if rule?
    (let [[a b] pages]
      [0 (update before a (fnil conj #{}) b)
       (update after b (fnil conj #{}) a)])
    (if (ordered? before after pages)
      [(+ total (midpoint pages)) before after]
      [total before after])))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (keep parse-line)
        (transduce (completing build-and-apply-rules)
                   [0 {} {}] (line-seq rdr))
        first)))

(defn page-comparator
  [before after a b]
  (cond
    (contains? (get before a) b) -1
    (contains? (get before b) a) 1
    (contains? (get after a) b) 1
    (contains? (get after b) a) -1
    :else 0))

(defn build-rules-and-fix-pages
  [[total before after] [rule? pages]]
  (if rule?
    (let [[a b] pages]
      [0 (update before a (fnil conj #{}) b)
       (update after b (fnil conj #{}) a)])
    (if (ordered? before after pages)
      [total before after]
      (let [cmp (partial page-comparator before after)]
        [(+ total (midpoint (vec (sort cmp pages)))) before after]))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (-> (keep parse-line)
        (transduce (completing build-rules-and-fix-pages)
                   [0 {} {}] (line-seq rdr))
        first)))
