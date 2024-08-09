(ns crinklywrappr.aoc.util
  (:require [clojure.string :as sg])
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

(defn chunk-seq
  "Lazily transforms an input seq, chunk by chunk.
     next-fn - takes an input seq which produces a 'chunk' from the seq.
     advance? - fn which takes the chunk and the input seq, and returns a
                boolean, whether to advance or not.
     xform - when advance? returns true, transforms the chunk
     rest-fn - takes the chunk and input seq, and 'advances' the input
               seq to the next chunk

  Chunks are _concated_ onto the seq.  If you need a collection in the seq,
  try wrapping the chunk in a vector."
  [next-fn advance? rest-fn xform]
  (fn chunk-seq' [xs]
    (let [chunk (next-fn xs)]
      (when (advance? chunk xs)
        (concat (xform chunk) (lazy-seq (chunk-seq' (rest-fn chunk xs))))))))

(defn chunky-line-seq [n xform ^BufferedReader rdr]
  (let [next-fn (fn [xs] (take n xs))
        advance? (fn [chunk _] (== (count chunk) 3))
        rest-fn (fn [_ xs] (drop n xs))
        f (chunk-seq next-fn advance? rest-fn xform)]
    (f (line-seq rdr))))

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
   (iteration
    (fn [from] (sg/index-of s sub from))
    :kf #(+ % adv) :initk 0)))

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
