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
       (let [pos (.getFilePointer raf)
             file-size (.length raf)]
         (if (< file-size len)
           (do
             ;; 1. read entire file
             (if (zero? (.getFilePointer raf))
               (.read raf b off len)
               (let [n (.read raf b off len)]
                 (.seek raf 0)
                 (.read raf b (+ off n) (- len n))))
             ;; 2. copy array repeatedly
             (loop [offset (+ off file-size)
                    written file-size
                    remaining (- len file-size)]
               (if (<= remaining written)
                 (do (System/arraycopy b off b offset remaining)
                     (.seek raf (mod (+ pos len) file-size))) ;; 3. seek to new position
                 (do (System/arraycopy b off b offset written)
                     (recur (+ offset written)
                            (+ written written)
                            (- remaining written))))))
           (let [n (.read raf b off len)]
             (when (< n len)
               (.seek raf 0)
               (.read raf b (+ off len) (- len n)))))
         len)))

    (available []
      (let [pos (.getFilePointer raf)
            end (.length raf)]
        (max 0 (- end pos))))

    (close []
      (.close raf))))
