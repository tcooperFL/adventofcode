(ns adventofcode2018.day1)

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/1

(def input-data "resources/2018/day1-input.txt")

(defn get-data
  "Parse the input file into an array of positive and negative numbers"
  []
  (read-string (str "[" (slurp input-data) "]")))

(defn freq
  "Sum up all the numbers to get a final frequency"
  []
  (reduce + (get-data)))

; (freq) => 490

; Part 2: Looping until we find a repeat
(defn twice-frequency
  "Compute intermediate frequencies from an infinite cycle of input data until
  you find a repeat. Return the first frequency that is generated twice."
  [data]
  (loop [seen #{} [f :as freqs] (reductions + (cycle data))]
    (if (seen f) f (recur (conj seen f) (drop 1 freqs)))))

; (twice-frequency (get-data)) -> 70357
; after 143 cycles!