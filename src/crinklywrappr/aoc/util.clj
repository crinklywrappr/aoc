(ns crinklywrappr.aoc.util
  (:require [clojure.edn :as edn]
            [clojure.string :as sg]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.readers.cyclic-input-stream :refer [cyclic-input-stream]])
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

(defprotocol TokenReader
  "Allows you to define a (possibly stateful) Reader that produces tokens.
  Tokens are produced from blocks.  A block may consist of one or more tokens."
  (next-block [_] "produces the next block.  nil represents the end of the stream.")

  (parse-block? [_ block] "should the block be parsed?")
  (parse-block [_ block] "parses the block into tokens")

  (include-token? [_ token] "should the token be included?")

  (continue? [_ block tokens] "should we continue reading?")
  (continue [_ block tokens] "continue reading.  returns a TokenReader."))

(defrecord DelimiterReader [^BufferedReader rdr ^Character delimiter]
  TokenReader
  (next-block [_]
    (let [bdel (byte delimiter)]
      (loop [sb (StringBuilder.)]
        (let [byte (.read rdr)]
          (cond
            (and (neg? byte) (zero? (.length sb))) nil
            ;; 10 is \newline
            (or (== byte 10) (neg? byte) (== byte bdel)) (sg/trim (str sb))
            :else (recur (.append sb (char byte))))))))
  (parse-block? [& _] true)
  (parse-block [_ block]
    (if (and (sg/starts-with? block "\"")
             (sg/ends-with? block "\""))
      [(edn/read-string block)]
      [block]))
  (include-token? [& _] true)
  (continue? [_ _ _] true)
  (continue [this _ _] this)
  Closeable
  (close [_] (.close rdr)))

(defn delimiter-reader [file delimiter]
  (->DelimiterReader (io/reader file) delimiter))

(letfn [(maybe-include-token [rdr token]
          (when (include-token? rdr token)
            token))]
  (defn token-seq [token-reader]
    (when-let [block (next-block token-reader)]
      (if-let [tokens (when (parse-block? token-reader block)
                        (keep #(maybe-include-token token-reader %)
                              (parse-block token-reader block)))]
        (if (continue? token-reader block tokens)
          (concat tokens (lazy-seq (token-seq (continue token-reader block tokens))))
          tokens)
        (lazy-seq (token-seq (continue token-reader block [])))))))

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

(defn split-string [i s & {:keys [omit?] :or {omit? false}}]
  (if omit?
    [(subs s 0 i) (subs s (min (count s) (inc i)))]
    [(subs s 0 i) (subs s i)]))

(defn hamming-distance [s1 s2]
  (apply + (map (fn [c1 c2] (if (= c1 c2) 0 1)) s1 s2)))
