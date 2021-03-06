(ns adventofcode2019.day1
  (:require [clojure.java.io :as io]))

(def input-data "resources/2019/day1-input.txt")

;; Calculate fuel required for a module
(defn simple-fuel-required [mass]
  (- (int (/ mass 3)) 2))

(defn apply-to-file [file f]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce +
            (->> rdr line-seq (map #(Integer/parseInt %)) (map f)))))

(defn part1 []
  (apply-to-file input-data simple-fuel-required))

;; Part1:
;; ==> 3252208

(defn recursive-fuel-required [mass]
  (reduce + (rest (take-while pos? (iterate simple-fuel-required mass)))))

(defn part2 []
  (apply-to-file input-data recursive-fuel-required))

;; Part2:
;; ==> 4875451