(ns crinklywrappr.aoc.2016.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as sg]))

(def file (io/resource "2016/day04.txt"))
(def pattern #"^([a-z\-]+)-(\d+)\[([a-z]+)\].*")
(def target-room "northpole object storage")

(defn parse [line]
  (rest (re-matches pattern line)))

(defn compute-checksum [encrypted-name]
  (letfn [(cmp [[a x] [b y]]
            (cond
              (< x y) 1
              (> x y) -1
              (< (int a) (int b)) -1
              (> (int a) (int b)) 1))]
    (as-> encrypted-name $
      (frequencies $)
      (dissoc $ \-)
      (sort cmp $)
      (take 5 $)
      (mapv first $)
      (apply str $))))

(defn valid? [[encrypted-name _ checksum]]
  (= (compute-checksum encrypted-name) checksum))

(defn valid-sector-ids [line]
  (let [[encrypted-name sector-id checksum :as room] (parse line)]
    (when (valid? room) (parse-long sector-id))))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (transduce (keep valid-sector-ids) + (line-seq rdr))))

(defn decrypt [[encrypted-name sector-id]]
  (let [sector-id' (parse-long sector-id)
        cycle-char (fn [c]
                     (if (= c \-)
                       \space
                       (char (+ (mod (+ (- (int c) 96) sector-id') 26) 96))))]
    [(apply str (mapv cycle-char encrypted-name)) sector-id']))

(defn northpole-object-storage? [line]
  (let [room (parse line)]
    (when (valid? room)
      (let [[decrypted-name sector-id] (decrypt room)]
        (when (= decrypted-name target-room)
          sector-id)))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (some northpole-object-storage? (line-seq rdr))))
