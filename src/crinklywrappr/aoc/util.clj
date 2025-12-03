(ns crinklywrappr.aoc.util
  (:require [clojure.edn :as edn]
            [clojure.string :as sg]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.readers.cyclic-input-stream :refer [cyclic-input-stream]]
            [crinklywrappr.aoc.readers.token-reader :as token])
  (:import [java.io BufferedReader Closeable File RandomAccessFile]
           [java.net URL URI]))

(defn char-seq
  [^BufferedReader rdr]
  (let [byte (.read rdr)]
    (when (pos? byte)
      (cons (char byte) (lazy-seq (char-seq rdr))))))

(defn wrap-line-seq
  "prepends and/or appends lines to the file when reading"
  ([^BufferedReader rdr before after]
   (cons before (lazy-seq (wrap-line-seq rdr after))))
  ([^BufferedReader rdr after]
   (if-let [line (.readLine rdr)]
     (cons line (lazy-seq (wrap-line-seq rdr after)))
     after)))

(defn token-seq [token-reader]
  (when-let [token (token/read-token token-reader)]
    (cons token (lazy-seq (token-seq token-reader)))))

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

(defn pluck
  "Returns a seq with the element in vector v at index i removed"
  [v i]
  (concat (subvec v 0 i)
          (subvec v (inc i))))

(defn extract-resource [url]
  (let [tmp (File/createTempFile "crinklywrappr-aoc" ".tmp")]
    (with-open [in (.openStream url)
                out (io/output-stream tmp)]
      (io/copy in out))
    (.deleteOnExit tmp)
    tmp))

(defn random-access-file-reader [^File file]
  (when (and (.isFile file) (.exists file))
    (RandomAccessFile. file "r")))

(defprotocol ExtraCoercions
  (^RandomAccessFile as-raf [x] "coerce to a read-only random access file"))

(extend-protocol ExtraCoercions
  nil
  (as-raf [_] nil)

  String
  (as-raf [s] (as-raf (io/file s)))

  File
  (as-raf [f] (random-access-file-reader f))

  URL
  (as-raf [u]
    (case (.getProtocol u)
      "file" (as-raf (io/file u))
      "jar" (as-raf (extract-resource u))
      (throw (IllegalArgumentException. (str "Not a file: " u)))))

  URI
  (as-raf [u] (as-raf (io/as-url u))))

(defn cyclic-reader [source]
  (-> source as-raf cyclic-input-stream io/reader))

(defn delimiter-reader
  "defaults to ,"
  [source & {:keys [delimiter] :or {delimiter \,}}]
  (-> source io/reader (token/delimiter-reader delimiter)))

(defn split-string [i s]
  [(subs s 0 i) (subs s i)])

(defn hamming-distance [s1 s2]
  (apply + (map (fn [c1 c2] (if (= c1 c2) 0 1)) s1 s2)))
