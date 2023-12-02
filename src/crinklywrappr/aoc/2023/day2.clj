(ns crinklywrappr.aoc.2023.day2
  (:require [clojure.java.io :as io]))

(def file (io/resource "2023/day2.txt"))

(def regex #"\d+|red|green|blue")

(defn parse-element [x]
  (if (re-matches #"\d+" x)
    (parse-long x) (keyword x)))

(defn maximum-counts [[game & reveals]]
  [game
   (reduce
    (fn [maximums [count color]]
      (if (> count (get maximums color))
        (assoc maximums color count)
        maximums))
    {:red 0 :green 0 :blue 0}
    (partition 2 reveals))])

(defn parse-line [line]
  (->> (re-seq regex line)
       (mapv parse-element)
       maximum-counts))

(defn part1 []
  (letfn [(possible-game? [[_ {:keys [red green blue]}]]
            (and (<= red 12) (<= green 13) (<= blue 14)))]
    (with-open [rdr (io/reader file)]
      (-> (comp (map parse-line)
             (filter possible-game?)
             (map first))
          (transduce + (line-seq rdr))))))

(defn part2 []
  (letfn [(power [[_ reveals]]
            (apply * (vals reveals)))]
    (with-open [rdr (io/reader file)]
      (-> (map (comp power parse-line))
          (transduce + (line-seq rdr))))))
