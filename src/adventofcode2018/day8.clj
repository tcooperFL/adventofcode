(ns adventofcode2018.day8)

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/8

(def input-data "resources/2018/day8-input.txt")

(def sample "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn get-data
  "Parse the string into a vector of numbers"
  [s]
  (read-string (str "[" s "]")))

(declare parse-tree)

(defn parse-siblings
  "Advance through the license to parse to-go nodes and return them in a vector"
  [license start-at parsed to-go]
  (if (zero? to-go)
    parsed
    (let [next (parse-tree license start-at)]
      (recur license (inc (:end-at next)) (conj parsed next) (dec to-go)))))

(defn parse-tree
  "Given the license and a start position, return a map with keys [start-at end-at children metadata]"
  [license start-at]
  (let [child-count (get license start-at)
        metadata-count (get license (inc start-at))
        children (parse-siblings license (+ start-at 2) [] child-count)
        metadata-start (if (zero? child-count)
                         (+ start-at 2)
                         (inc (:end-at (last children))))
        end-at (dec (+ metadata-start metadata-count))]
    {:start-at start-at
     :end-at end-at
     :children children
     :metadata (map #(get license %) (range metadata-start (inc end-at)))}))

(defn sum-metadata
  "Walk the tree and sum up the metadata"
  [node]
  (reduce + (apply + (:metadata node)) (map sum-metadata (:children node))))

(defn part1 [license]
  (sum-metadata (parse-tree license 0)))

; (part1 (get-data (slurp input-data)))
; => 41521

; Part 2

(defn value-of
  "See the somewhat convoluted definition at https://adventofcode.com/2018/day/8#part2"
  [node]
  (if (empty? (:children node))
    (reduce + (:metadata node))
    (reduce + (map value-of (map #(get (:children node) (dec %)) (:metadata node))))))

(defn part2 [license]
  (value-of (parse-tree license 0)))

; (part2 (get-data (slurp input-data)
; =>
