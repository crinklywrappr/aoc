(ns crinklywrappr.aoc.readers.cyclic-input-stream
  (:import (java.io InputStream)
           (java.io RandomAccessFile)))

(defn cyclic-input-stream
  "Return an InputStream that reads bytes from a RandomAccessFile (RAF)
   and when EOF is reached, automatically seeks back to the beginning.
   Closing the stream also closes the RAF."
  [^RandomAccessFile raf]
  (proxy [InputStream] []
    (read
      ([]
       (let [b (.read raf)]
         (if (neg? b)
           (do
             (.seek raf 0)
             (.read raf))
           b)))
      ([b]
       (.read this b 0 (alength ^bytes b)))
      ([^bytes b off len]
       (let [n (.read raf b off len)]
         (if (neg? n)
           (do
             (.seek raf 0)
             (.read raf b off len))
           n))))

    (available []
      (let [pos (.getFilePointer raf)
            end (.length raf)]
        (max 0 (- end pos))))

    (close []
      (.close raf))))
