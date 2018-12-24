(ns adventofcode2018.day10
  (:require [clojure.java.io :as io]
            [incanter.core :ref :all]
            [incanter.charts :ref :all]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/10

(def input-data "resources/2018/day10-input.txt")
(def sample-data "resources/2018/day10-sample.txt")

(def line-pattern #"position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>")

(defn get-data
  "Parse the data into pairs: [X Y] where X must come before Y"
  [file]
  (with-open [rdr (io/reader file)]
    (doall
      (map (fn [line]
             (partition 2 (map read-string (rest (re-matches line-pattern line)))))
           (line-seq rdr)))))

(defn calc-at
  "A function that computes the points to turn on at time t"
  [data i]
  (map (fn [[p v]] (map #(+ %1 (* i %2)) p v)) data))

(defn part1
  "Use a dynamic scatter plot with a slider to see the time changes dynamically"
  [data n]
  (view (dynamic-scatter-plot [t (range n)] (calc-at data t))))

;; (part2 (get-data input-data) 100000) and then scrolled until I saw the word come into focus at 10101
;; => GPEPPPEJ

;; Part 2
;; => It came into focus at t=10101