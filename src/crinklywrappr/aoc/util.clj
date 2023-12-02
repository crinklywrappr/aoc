(ns crinklywrappr.aoc.util
  (:import [java.io BufferedReader]))

(defn char-seq
  "ascii-only support"
  [^BufferedReader rdr]
  (let [byte (.read rdr)]
    (when (pos? byte)
      (cons (char byte)
            (lazy-seq (char-seq rdr))))))
