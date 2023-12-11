(ns crinklywrappr.aoc.util
  (:require [clojure.string :as sg]
            [clojure.core.reducers :as r])
  (:import [java.io BufferedReader]))

(defn char-seq
  [^BufferedReader rdr]
  (let [byte (.read rdr)]
    (when (pos? byte)
      (cons (char byte) (lazy-seq (char-seq rdr))))))

(defn wrap-line-seq
  "prepends and/or appends a line to the file when reading"
  ([^BufferedReader rdr before after]
   (cons before (lazy-seq (wrap-line-seq rdr after))))
  ([^BufferedReader rdr after]
   (if-let [line (.readLine rdr)]
     (cons line (lazy-seq (wrap-line-seq rdr after)))
     [after])))

;; Stolen from Vincent Ho, who stole it from SO
(defn re-pos [re s]
  (if (nil? re)
    {}
    (loop [m (re-matcher re s)
           res {}]
      (if (.find m)
        (recur m (assoc res (.start m) (.group m)))
        res))))

(defn string-indices
  ([sub s]
   (string-indices sub s (count sub)))
  ([sub s adv]
   (->> (iteration
         (fn [from] (sg/index-of s sub from))
         :kf #(+ % adv) :initk 0)
        (r/fold conj))))

(defn gcd
  ([] 0)
  ([x] x)
  ([x y]
   (if (zero? y)
     x
     (recur y (mod x y))))
  ([x y & more]
   (reduce gcd (gcd x y) more)))

(defn lcm
  ([] 1)
  ([x] x)
  ([x y]
   (/ (abs (*' x y))
      (gcd x y)))
  ([x y & more]
   (reduce lcm (lcm x y) more)))
