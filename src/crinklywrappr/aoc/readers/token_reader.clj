(ns crinklywrappr.aoc.readers.token-reader
  (:require [clojure.string :as sg])
  (:import (java.io Closeable)))

(defprotocol TokenReader
  (read-token [this] "Read the next token from the stream or reader"))

(defn delimiter-reader [rdr ^Character delimiter]
  (let [bdel (byte delimiter)]
    (reify
      TokenReader
      (read-token [_]
        (loop [sb (StringBuilder.)]
          (let [b (.read rdr)]
            (cond
              (and (neg? b) (zero? (.length sb))) nil
              (or (== b 10) (neg? b) (== b bdel)) (sg/trim (str sb))
              :else (recur (.append sb (char b)))))))
      Closeable
      (close [_] (.close rdr)))))

