(ns adventofcode2017.day15)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/15
;; Part 1

; Given
(def seed-a 512)
(def seed-b 191)

(def ^:const factor-a 16807)
(def ^:const factor-b 48271)
(def ^:const divisor 2147483647)

; Infinite lazy sequence from a generator
(defn generator [seed factor]
  (let [next (mod (* seed factor) divisor)]
    (cons next (lazy-seq (generator next factor)))))

; Infinite sequence of pairs to compare
(defn generate-pairs [a b]
  (map vector (generator seed-a factor-a) (generator seed-b factor-b)))

; A bit mask that extracts the lower 16 bits
(def ^:const one16 (Integer/parseInt "1111111111111111", 2))
(defn lower-part [n] (bit-and n one16))

; Loop through n pairs an count up the number with the same lower-parts.
(defn count-matches [n pairs]
  (loop [i n
         [[a b] & rest] pairs
         matches 0]
    (if (zero? i)
      matches
      (if (= (lower-part a) (lower-part b))
        (recur (dec i) rest (inc matches))
        (recur (dec i) rest matches)))))

(defn part1 []
  (count-matches 40000000 (generate-pairs seed-a seed-b)))

; (part1)
; => 567

;; Part 2

; 5 million pairs, a values divisible by 4, b values divisible by 8
(defn part2 []
  (count-matches 5000000
                 (map vector
                      (filter #(zero? (mod % 4)) (generator seed-a factor-a))
                      (filter #(zero? (mod % 8)) (generator seed-b factor-b)))))
; (part2)
; => 323
