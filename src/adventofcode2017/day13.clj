(ns adventofcode2017.day13)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/13
;; Part 1

(def day13-input-file "resources/2017/day13-input.txt")

(defn caught? [[i n]]
  (zero? (mod i (* 2 (dec n)))))

(defn severity [[i n]]
  (* i n))

(defn make-pair [s]
  (map #(Integer/parseInt %) (re-seq #"\d+" s)))

(defn part1 [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> rdr line-seq
         (transduce (comp (map make-pair) (filter caught?) (map severity)) + 0))))

; (part1 day13-input-file)
; => 748

(defn passed?
  ([pairs] (not-any? caught? pairs))
  ([pairs delta] (passed? (map (fn [[i n]] [(+ i delta) n]) pairs))))

(defn part2 [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (let [pairs (map make-pair (line-seq rdr))]
      (some #(if (passed? pairs %) %) (range)))))

; (part2 day13-input-file)
; => 3873662
