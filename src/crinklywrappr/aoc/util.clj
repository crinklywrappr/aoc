(ns crinklywrappr.aoc.util
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
