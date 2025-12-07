(ns crinklywrappr.aoc.2025.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2025/day06.txt"))

(defn parse-line [line]
  (let [tokens (sg/split line #"\s+")]
    (if (re-matches #"\d+" (first tokens))
      [:numbers (mapv parse-long tokens)]
      [:operators tokens])))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce
     (map parse-line)
     (completing
      (fn [totals [id tokens]]
        (if (= id :numbers)
          (mapv (fn [[sum product] n] [(+ sum n) (* product n)]) totals tokens)
          (mapv (fn [[sum product] op] (if (= op "+") sum product)) totals tokens)))
      (fn [totals] (apply + totals)))
     (repeat [0 1]) (line-seq rdr))))

(defn parse-line2 [line]
  (let [operators? (or (sg/starts-with? line "+")
                       (sg/starts-with? line "*"))]
    (as-> (if operators?
            (util/re-pos #"\*|\+" line)
            (util/re-pos #"\d" line)) $
      (seq $) (flatten $) (apply sorted-map $)
      (if operators?
        (assoc $ (inc (count line)) "|")
        $))))

(defn build-numbers [parsed-lines i]
  (-> (keep #(get % i))
      (transduce str parsed-lines)
      parse-long))

(defn calc-total [parsed-lines [[from op] [to]]]
  (-> (map (partial build-numbers parsed-lines))
      (transduce (if (= op "+") + *) (range from (dec to)))))

(defn part2 []
  (let [parsed-lines (with-open [rdr (io/reader file)] (mapv parse-line2 (line-seq rdr)))]
    (-> (map (partial calc-total (butlast parsed-lines)))
        (transduce + (partition 2 1 (last parsed-lines))))))
