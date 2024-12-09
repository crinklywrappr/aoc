(ns crinklywrappr.aoc.2024.day09
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(defn block-size [x] (- (long x) 48))

(def file (mapv block-size (butlast (slurp (io/resource "2024/day09.txt")))))

(defn checksum [total file-id index file-size]
  (reduce (fn [a b] (+ a (* file-id b))) total (range index (+ index file-size))))

(defn part1 []
  (loop [total 0 file-id 0 i 0
         [block-sz & xs] file
         [[file-id' file-sz] & xs' :as rfiles] (reverse (map-indexed vector (take-nth 2 file)))
         k :file]
    (cond
      (== file-id file-id') (checksum total file-id' i file-sz)
      (= k :file)
      (recur (checksum total file-id i block-sz)
             (inc file-id) (+ i block-sz) xs rfiles :free)
      (= k :free)
      (let [sz (min block-sz file-sz)]
        (recur (checksum total file-id' i sz)
               file-id (+ i sz) (if (== sz block-sz) xs (cons (- block-sz sz) xs))
               (if (== sz file-sz) xs' (cons [file-id' (- file-sz sz)] xs'))
               (if (== sz block-sz) :file :free))))))

(defn defrag [descriptors [_ _ sz :as file]]
  (loop [descriptors' [] [[tag _ sz' :as dx] & xs] descriptors visited? false moved? false]
    (cond
      (nil? dx) descriptors'
      (and moved? (= dx file)) (recur (conj descriptors' [\e 0 sz]) xs true true)
      (= dx file) (recur (conj descriptors' dx) xs true false)

      (and (= tag \e) (not visited?) (not moved?) (< sz sz'))
      (recur (conj descriptors' file [\e 0 (- sz' sz)]) xs false true)

      (and (= tag \e) (not visited?) (not moved?) (== sz sz'))
      (recur (conj descriptors' file) xs false true)

      :else (recur (conj descriptors' dx) xs visited? moved?))))

(defn calculate-checksum [[total index] [tag file-id block-size]]
  (if (= tag \e)
    [total (+ index block-size)]
    [(checksum total file-id index block-size) (+ index block-size)]))

(defn part2 []
  (let [descriptors (map-indexed (fn [i x] (if (zero? (mod i 2)) [\f (/ i 2) x] [\e 0 x])) file)
        files (reverse (rest (take-nth 2 descriptors)))]
    (->> files
         (reduce defrag descriptors)
         (reduce calculate-checksum [0 0])
         first)))
