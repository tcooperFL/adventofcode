(ns adventofcode2018.day14
  (:require [clojure.java.io :as io]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/14

(def input-data 236021)

(defn combine
  "Return the digits of the sum of a and b where both are between 0 and 9"
  [a b]
  (let [sum (+ a b)]
    (if (> sum 9) [1 (rem sum 10)] [sum])))

(defn display
  "Debug display the line in the format provided as examples"
  [board x y]
  (doseq [i (range (count board))]
    (condp = i
      x (print (str "(" (board i) ")"))
      y (print (str "[" (board i) "]"))
      (print (str " " (board i) " "))))
  (println))

(defn run
  "Create and score n recipes and return the 10 recipe scores after last one created."
  [term-fn]
  (loop [size 2 scoreboard (transient [3 7]) r1 0 r2 1]
    #_ (display scoreboard r1 r2)
    (if (term-fn scoreboard)
      (persistent! scoreboard)
      (let [s1 (scoreboard r1)
            s2 (scoreboard r2)
            digits (combine s1 s2)
            board1 (conj! scoreboard (first digits))]
        (if (and (> (count digits) 1) (term-fn board1))
          (persistent! board1)
          (let [next-board (if (> (count digits) 1) (conj! board1 (second digits)) board1)
                board-size (+ size (count digits))]
            (recur board-size
                   next-board
                   (rem (+ r1 s1 1) board-size)
                   (rem (+ r2 s2 1) board-size))))))))

(defn term
  "Return a termination function that stops when the board reaches n + 10 in size"
  [n]
  (fn [board]
    (>= (count board) (+ n 10))))

(defn part1 [n]
  (let [scoreboard (run (term n))]
    (println (apply str (take 10 (subvec scoreboard n))))))

;; (part1 input-data)
;; => 6297310862

;; Part 2

(defn term2
  "Returns a function that returns true if the end of the board matches these digits"
  [pattern]
  (let [digits (map #(- (int %) (int \0)) pattern)
        size (count digits)]
    (fn [board]
      (let [board-size (count board)]
        (every? (fn [[x y]] (= x y))
                (map #(vector %1 (nth board %2 :na))
                     digits (range (- board-size size) board-size)))))))

(defn part2 [pattern]
  (let [scoreboard (run (term2 pattern))]
    (- (count scoreboard) (count pattern))))

;; (part2 (str input-data))
;; => 20221334

