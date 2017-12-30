(ns adventofcode.day14
  (:require [adventofcode.day10 :refer [part2] :rename {part2 knot-hash}]
            [adventofcode.day12 :refer [partition-size]]))

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/14
;; Part 1

(def keystring "hxtvlmkl")
(def hash-inputs (map #(str keystring "-" %) (range 128)))

; Calculate the 1's in the binary form of this hash string
(defn used-in [s]
  (->> (clojure.string/split s #"")
       (map #(Integer/parseInt % 16))
       (map #(Integer/toBinaryString %))
       (map #(re-seq #"1" %))
       (map count)
       (reduce +)))

; Sum up the 1's in the binary representations of all our hash inputs.
; Use day10's part2 solution for knot-hash, and compute them all in parallel
(defn part1 [inputs]
  (->> inputs (pmap knot-hash) (map used-in) (reduce + 0)))

; (part1 hash-inputs)
; => 8214

;; Part 2

; Given a hash, return an array of locations where 1's are, left to right, 0-based.
(defn ones-at [s]
  (transduce
    (comp (map #(Integer/parseInt % 16))
          (mapcat #(format "%4s" (Integer/toBinaryString %)))
          (map-indexed vector)
          (filter #(= (second %) \1))
          (map first))
    conj
    (clojure.string/split s #"")))

; Form a sparse graph as a set of [x,y] grid coords of 1's in the hash input
(defn graph [inputs]
  (->> inputs
       (pmap knot-hash)
       (map ones-at)
       (map-indexed (fn [r cols] (map (fn [c] [r c]) cols)))
       (apply concat)
       set))

; Given a point, return neighboring points that exist (had a 1 in them)
(defn neighbors [g [x y :as point]]
  (filter g [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

; Based on day12 solution, and using its partition-size function
(defn part2 [inputs]
  (let [g (graph inputs)
        neighbor-fn (partial neighbors g)
        visited (transient #{})
        remember-fn (partial conj! visited)]
    (count
      (for [p g :when (not (visited p))]
        (do
          (remember-fn p)
          (partition-size p neighbor-fn visited remember-fn))))))

; => 1093