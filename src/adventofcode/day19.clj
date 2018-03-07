(ns adventofcode.day19
  (:require [clojure.core.async :refer [>! >!! <! <!! go chan buffer close! thread alts! alts!! timeout]]))

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/18
;; Part 1

;; Problem setup data
(def input-file "resources/day19-input.txt")

(def sample "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ \n")

(defn index-nonspaces
  "Given one character row of the diagram, return a seq of position and content for non-blanks."
  [s]
  (remove #(= (second %) \space) (map-indexed list (seq s))))

(defn compile-row
  "Given a [row-index ((col content)...)], return a map of [r c] => content"
  [[r row]]
  (reduce (fn [m [c content]]
            (assoc m [r c] content))
          {}
          (index-nonspaces row)))

(defn compile-map
  "Given a string description of a diagram with newlines, return a map of row, col contents for non-blanks"
  [s]
  (->> (clojure.string/split s #"\n")
       (map-indexed vector)
       (map compile-row)
       (reduce merge)))

(defn entry-point
  "Return the (r, c) of the starting point of the path in this diagram"
  [m]
  (->> m
       (filter (fn [[[r _] ch]] (and (= r 0) (= ch \|))))
       (sort-by (comp second first))
       first
       first))

(def directions (partition 2 1 (take 5 (cycle [-1 0 1 0]))))

(defn neighbors
  "Return a sequence of coordinates of all the cells around this one"
  [coords]
  (map #(map + coords %) directions))

(defn next-coords
  "Compute the next coords based on current direction, previous coords, and cmd"
  [m from coords dir cmd]
  (if (= cmd \+)
    (some #(if (m %) %) (remove #{from} (neighbors coords)))
    (map + coords dir)))

(defn path
  "Produce a sequence of coordinates that walk through this diagram."
  ([m] (path m [1 0] nil (entry-point m)))
  ([m direction from coords]
   (if-let [cell (get m coords)]
     (cons coords
           (lazy-seq
             (let [next (next-coords m from coords direction cell)]
               (path m (map - next coords) coords next)))))))

(defn letters
  "Walk the diagram and return the string of letters found on the path"
  [s]
  (let [m (compile-map s)]
    (->> (path m)
         (map m)
         (filter #(java.lang.Character/isLetter %))
         (apply str))))

(defn part1 []
  (->> input-file slurp letters))

; (part1)
; => "GINOWKYXH"

(defn part2 []
  (->> input-file slurp compile-map path count))

; (part2)
; => 16636
