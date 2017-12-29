(ns adventofcode.day12)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/12
;; Part 1

; Find the number of reachable nodes in an undirected graph from a given node.

(def day12-input-file "resources/day12-input.txt")

; Create node from one-line description
(defn create-edge-list [s]
  (vec (map #(Integer/parseInt %) (rest (re-seq #"\d+" s)))))

; Load the graph and return it as a vector of edge vectors.
; This assumes the graph nodes appear in the file one per line, 0 to n.
(defn load-graph [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (let [lines (line-seq rdr)]
      (vec (map create-edge-list lines)))))

; Compute graph partition that includes this node, returning the number of nodes in the partition
(defn partition-count
  ([g n]
   (let [visited (transient #{})] (partition-count g n visited (partial conj! visited))))
  ([g n seen remember]
   (reduce +
           (for [next (g n) :when (not (seen next))]
             (do
               (remember next)
               (inc (partition-count g next seen remember)))))))

; Load the input graph and count the partition containing node 0
(defn part1 [input-file]
  (partition-count (load-graph input-file) 0))

; (part1 day12-input-file)
; => 134

;; Part 2

; Count the partitions.
(defn part2 [input-file]
  (let [g (load-graph input-file)
        visited (transient #{})
        remember (partial conj! visited)]
    (count
      (for [i (range (count g)) :when (not (visited i))]
        (do
          (remember i)
          (partition-count g i visited remember))))))

; (part2 day12-input-file
; => 193