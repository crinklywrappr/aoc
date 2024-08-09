(ns crinklywrappr.aoc.2016.day05
  (:require [clj-commons.digest :as digest]
            [clojure.string :as sg]))

(defn next-key [prefix expectation]
  (fn next-key' [[code found] suffix]
    (if (> found 7)
      (reduced (apply str code))
      (let [hash (digest/md5 (str prefix suffix))]
        (if (sg/starts-with? hash expectation)
          [(conj code (get hash 5)) (inc found)]
          [code found])))))

(defn part1 []
  (reduce (next-key "ugkcyxxp" "00000") [[] 0] (range)))

(defn next-key-door2 [prefix expectation]
  (fn next-key' [[code found] suffix]
    (if (> found 7)
      (reduced (apply str code))
      (let [hash (digest/md5 (str prefix suffix))
            pos (- (int (get hash 5)) 48)]
        (if (and (sg/starts-with? hash expectation)
                 (<= 0 pos 7) (nil? (get code pos)))
          [(assoc code (- (int (get hash 5)) 48) (get hash 6)) (inc found)]
          [code found])))))

(defn part2 []
  (reduce (next-key-door2 "ugkcyxxp" "00000")
          [(vec (repeat 8 nil)) 0] (range)))
