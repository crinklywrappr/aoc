(ns crinklywrappr.aoc.2015.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]))

(def file (io/resource "2015/day05.txt"))

(defn count-nice-lines
  [pred reader]
  (reduce
   (fn f [acc line]
     (if (pred line)
       (inc acc) acc))
   0 (line-seq reader)))

(defn part1 []
  (let [vowels #"a|e|i|o|u"
        required (->> (range 97 123)
                      (mapv #(str (char %) (char %)))
                      (sg/join \|) re-pattern)
        denied #"ab|cd|pq|xy"]
    (with-open [rdr (io/reader file)]
      (count-nice-lines
       (fn [line]
         (and (>= (count (re-seq vowels line)) 3)
              (seq (re-seq required line))
              (empty? (re-seq denied line))))
       rdr))))

(defn part2 []
  (let [repeating (->> (for [x (range 97 123)
                             y (range 97 123)
                             :let [xc (char x)
                                   yc (char y)]]
                         (format ".*%s%s.*%s%s.*"
                                 xc yc xc yc))
                       (sg/join \|) re-pattern)
        sandwiched (->> (for [x (range 97 123)
                              y (range 97 123)
                              :let [xc (char x)
                                    yc (char y)]]
                          (str xc yc xc))
                        (sg/join \|) re-pattern)]
    (with-open [rdr (io/reader file)]
      (count-nice-lines
       (fn [line]
         (and (some? (re-matches repeating line))
              (seq (re-seq sandwiched line))))
       rdr))))
