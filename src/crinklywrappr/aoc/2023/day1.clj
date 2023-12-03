(ns crinklywrappr.aoc.2023.day1
  (:require [clojure.java.io :as io]))

(def file (io/resource "2023/day1.txt"))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (-> (map (comp parse-long
                (partial apply str)
                (juxt first last)
                (partial re-seq #"\d")))
        (transduce + (line-seq rdr)))))

(defn part2 []
  (let [rgx #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))"
        xf {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9
            "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}]
    (with-open [rdr (io/reader file)]
      (-> (map (comp (partial apply +)
                  (juxt (comp (partial * 10) xf first)
                        (comp xf last))
                  (partial mapv second)
                  (partial re-seq rgx)))
          (transduce + (line-seq rdr))))))

