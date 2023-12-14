(ns crinklywrappr.aoc.2023.day08
  (:require [clojure.string :as sg]
            [clojure.java.io :as io]
            [crinklywrappr.aoc.util :as util]))

(def file (io/resource "2023/day08.txt"))

(defn parse-line [line]
  (let [[k l r] (re-seq #"[1-9A-Z]+" line)]
    [k {\L l \R r}]))

(defn part1 []
  (with-open [rdr (io/reader file)]
    (let [[line & lines] (line-seq rdr)
          instructions (cycle (seq line))]
      (loop [steps 0 routes {} target "AAA"
             [instr & ri :as instrs] instructions
             [line & rl :as lines] (rest lines)]
        (cond
          (= target "ZZZ") steps

          (contains? routes target)
          (recur (inc steps) routes
                 (get-in routes [target instr])
                 ri lines)

          :else (let [[k v] (parse-line line)]
                  (recur steps (assoc routes k v) target instrs rl)))))))

(defn part2 []
  (letfn [(a? [target] (sg/ends-with? target "A"))
          (z? [target] (sg/ends-with? target "Z"))
          (movable? [routes [id {:keys [target] :as ghost}]]
            (when (contains? routes target) [id ghost]))]
    (with-open [rdr (io/reader file)]
      (let [[line & lines] (line-seq rdr)
            instructions (cycle (seq line))]
        (loop [lcm 1 gc 0 ghosts {} routes {} [line & rl :as lines] (rest lines)]
          (let [[id {:keys [steps target instrs] :as ghost}] (some (partial movable? routes) ghosts)]
            (cond
              (and (nil? line) (empty? ghosts)) lcm

              (some? ghost) (let [new-target (get-in routes [target (first instrs)])]
                              (if (z? new-target)
                                (recur (util/lcm lcm (inc steps)) gc (dissoc ghosts id) routes lines)
                                (recur lcm gc
                                       (-> ghosts
                                           (update-in [id :steps] inc)
                                           (update-in [id :instrs] rest)
                                           (assoc-in [id :target] new-target))
                                       routes lines)))

              :else (let [[k v] (parse-line line)
                          new-routes (assoc routes k v)]
                      (if (a? k)
                        (recur lcm (inc gc)
                               (assoc ghosts gc {:steps 0 :target k :instrs instructions})
                               new-routes rl)
                        (recur lcm gc ghosts new-routes rl))))))))))
