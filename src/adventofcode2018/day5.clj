(ns adventofcode2018.day5
  (:require [clojure.string :refer [trim upper-case]]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/5

(def input-data "resources/2018/day5-input.txt")
(def sample "dabAcCaCBAcCcaDA")

(defn get-data
  "Parse the data into a sequence of lines"
  []
  (trim (slurp input-data)))

(defn cancelling?
  "True if these to characters cancel each other: same letter, different capitalization"
  [x y]
  (and (not= x y) (char? x) (char? y) (= (upper-case x) (upper-case y))))

(defn react
  "Make a single pass through the string, collapsing opportunistically"
  [polymer]
  (loop [done '() todo (seq polymer)]
    (if (empty? todo)
      (count done)
      (recur
        (if (cancelling? (first todo) (first done))
          (rest done) (cons (first todo) done))
        (rest todo)))))

; (react (get-data))
; => 11814

;; Part 2
(defn min-polymer
  "Find size of the smallest reacted polymer of the 26 variations with each letter removed"
  [data]
  (let [coll (seq data)]
    (apply min
           (->>
             (map #(char (+ (int \A) (int %))) (range 26))  ; Capital alphabet
             (map (fn [c] (remove #(= (upper-case %) (upper-case c)) coll))) ; Each removed
             (map react)))))                                ; Size of each after reactions

; (min-polymer (get-data))
; => 4282

