(ns crinklywrappr.aoc.2024.day13
  (:require [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]
            [crinklywrappr.aoc.geom :as geom]))

(def file (io/resource "2024/day13.txt"))
(def ^:dynamic modifier 0)

;; there's a case where `geom/intersection` => -1 which this does not handle

(defn parse-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn calculate-intersection [[ax ay] [bx by] [cx cy]]
  (let [line1 [[0 0] [cx (* (/ ay ax) cx)]]
        line2 [[(/ (- (/ (* by cx) bx) cy) (/ by bx)) 0] [cx cy]]]
    (geom/intersection line1 line2)))

(defn calculate-tokens [[[ax ay :as a] [bx by :as b] [cx cy :as c]]]
  (let [[cx cy :as c] [(+ cx modifier) (+ cy modifier)]
        [x y] (calculate-intersection a b c)]
    (if (every? zero? [(rem x ax) (rem (- cx x) bx)
                       (rem y ay) (rem (- cy y) by)])
      (+ (* 3 (/ x ax)) (/ (- cx x) bx))
      0)))

(defn get-fewest-tokens [[total state] line]
  (if (seq line)
    [total (conj state (parse-line line))]
    [(+ total (calculate-tokens state)) []]))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (first
     (reduce get-fewest-tokens [0 []]
             (util/wrap-line-seq rdr [""])))))

(defn part2 []
  (with-open [rdr (io/reader file)]
    (binding [modifier 10000000000000N]
      (first
       (reduce get-fewest-tokens [0 []]
               (util/wrap-line-seq rdr [""]))))))
