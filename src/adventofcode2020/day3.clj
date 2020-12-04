(ns adventofcode2020.day3
  (:require [clojure.java.io :as io]))

(def input-data "resources/2020/day3-input.txt")

(defn solve [file reducer f]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce reducer (rest (map-indexed f (map cycle (line-seq rdr)))))))

(defn tree-count
  [multiplier row s]

  (if (= \# (nth s (* row multiplier))) 1 0))

(defn part1
  "Read lines of the file and sum up the tree hits per line"
  [file]
  (solve file + (partial tree-count 3)))

;;-> 262

(defn forest-count
  [row s]
  (let [f #(tree-count % row s)]
    [(f 1) (f 3) (f 5) (f 7) (if (even? row) (f 1) 0)]))

(defn collect-counts [acc result]
  (map + acc result))

(defn part2
  [file]
  (apply * (solve file collect-counts forest-count)))

;;=> 2698900776
