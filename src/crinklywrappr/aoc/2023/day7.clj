(ns crinklywrappr.aoc.2023.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]))

(def file (io/resource "2023/day7.txt"))

(defn winnings [idx [_ bid]] (* bid (inc idx)))
(defn parse-hex [hex-str] (Long/parseLong hex-str 16))

(defn parse [line]
  (let [[hand bid] (sg/split line #" ")]
    [hand (parse-long bid)]))

(defn hand-type' [card-freqs]
  (case card-freqs
    [5] 6
    [4 1] 5
    [1 4] 5
    [3 2] 4
    [2 3] 4
    [3 1 1] 3
    [1 3 1] 3
    [1 1 3] 3
    (case (count card-freqs)
      3 2
      4 1
      5 0)))

(defn compare-hands [hand-type score-cards]
  (fn compare-cards' [a b]
    (let [ta (hand-type a)
          tb (hand-type b)]
      (cond
        (< ta tb) -1
        (> ta tb) 1
        :else (let [sa (score-cards a)
                    sb (score-cards b)]
                (cond
                  (< sa sb) -1
                  (> sa sb) 1
                  :else 0))))))

(defn part1 []
  (letfn [(hand-type [[hand _]]
            (-> hand frequencies
                vals vec hand-type'))
          (score-cards [[hand _]]
            (-> hand
                (sg/replace "A" "E")
                (sg/replace "K" "D")
                (sg/replace "Q" "C")
                (sg/replace "J" "B")
                (sg/replace "T" "A")
                parse-hex))]
    (with-open [rdr (io/reader file)]
      (->> rdr line-seq
           (mapv parse)
           (sort (compare-hands
                  (memoize hand-type)
                  (memoize score-cards)))
           (map-indexed winnings)
           (apply +)))))

(defn part2 []
  (letfn [(hand-type [[hand _]]
            (let [freqs (frequencies hand)
                  jacks (get freqs \J)
                  score (-> freqs vals vec
                            hand-type')]
              (case [(some? jacks) score]
                [true 5] 6
                [true 4] 6
                [true 3] 5
                [true 1] 3
                [true 0] 1
                (case [jacks score]
                  [2 2] 5
                  [1 2] 4
                  score))))
          (score-cards [[hand _]]
            (-> hand
                (sg/replace "A" "D")
                (sg/replace "K" "C")
                (sg/replace "Q" "B")
                (sg/replace "J" "1")
                (sg/replace "T" "A")
                parse-hex))]
    (with-open [rdr (io/reader file)]
      (->> rdr line-seq
           (mapv parse)
           (sort (compare-hands
                  (memoize hand-type)
                  (memoize score-cards)))
           (map-indexed winnings)
           (apply +)))))
