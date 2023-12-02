(ns crinklywrappr.aoc.util
  (:import [java.io BufferedReader]))

(defn char-seq
  "ascii-only support"
  [^BufferedReader rdr]
  (let [byte (.read rdr)]
    (when (pos? byte)
      (cons (char byte)
            (lazy-seq (char-seq rdr))))))

(defn reduce-while
  ([whilef accf coll]
   (reduce-while whilef accf (first coll) (rest coll)))
  ([whilef accf init coll]
   (letfn [(pick-value [[prior acc coll]]
             (if (whilef acc) acc prior))]
     (->> [nil init coll]
          (iterate (fn stepf [[prior acc coll]]
                     [acc (accf acc (first coll)) (rest coll)]))
          (drop-while (fn somef [[prior acc coll]]
                        (and (seq coll) (whilef acc))))
          first pick-value))))
