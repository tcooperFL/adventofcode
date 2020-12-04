(ns adventofcode2020.day1
  (:require [clojure.java.io :as io]))

(def input-data "resources/2020/day1-input.txt")

(defn get-data
  "Parse the input file into an array of numbers"
  []
  (read-string (str "[" (slurp input-data) "]")))

(defn pairs
  "Create a lazy sequence of all pairs from this collection"
  [c]
  (for [x c y c] [x y]))

(defn solve
  "Solve the problem with a given tupler function"
  [tupler]
  (some (fn [tuple]
          (if (= 2020 (apply + tuple))
            (apply * tuple)))
        (tupler (get-data))))

(defn part1 []
  (solve pairs))

;; => 956091

(defn triples
  "Create a lazy sequence of all triples from this collection"
  [c]
  (for [x c y c z c] [x y z]))

(defn part2 []
  (solve triples))

;; => 79734368
