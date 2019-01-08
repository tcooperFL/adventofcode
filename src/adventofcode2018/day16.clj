(ns adventofcode2018.day16
  (:require [clojure.java.io :as io]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/16

(def input-data "resources/2018/day16-input.txt")

(defn create-sample [input [code a b c] output]
  {:code   code
   :input  (reduce conj {} (map-indexed vector input))
   :output (reduce conj {} (map-indexed vector output))
   :a      a
   :b      b
   :c      c})

(defn parse-before [s] (read-string (apply str `("[" ~@(re-find #"\d+, \d+, \d+, \d+" s), "]"))))
(defn parse-instruction [s] (read-string (apply str `("[" ~@(re-find #"\d+ \d+ \d+ \d+" s) "]"))))
(defn parse-after [s] (parse-before s))

(defn load-samples
  "Parse the input file to return a sequence of instruction samples
  {:id <id> :input [..] :instruction [..] :output [..]}"
  [file]
  (with-open [rdr (io/reader file)]
    (loop [[before instruction after & rest] (line-seq rdr)
           samples []]
      (if (nil? before)
        samples
        (recur (drop-while #(and % (not (.startsWith % "Before:"))) rest)
               (conj samples (create-sample (parse-before before)
                                            (parse-instruction instruction)
                                            (parse-after after))))))))

; Opcodes
(defn addr [a b c r] (assoc r c (+ (r a) (r b))))
(defn addi [a b c r] (assoc r c (+ (r a) b)))
(defn mulr [a b c r] (assoc r c (* (r a) (r b))))
(defn muli [a b c r] (assoc r c (* (r a) b)))
(defn banr [a b c r] (assoc r c (bit-and (r a) (r b))))
(defn bani [a b c r] (assoc r c (bit-and (r a) b)))
(defn borr [a b c r] (assoc r c (bit-or (r a) (r b))))
(defn bori [a b c r] (assoc r c (bit-or (r a) b)))
(defn setr [a _ c r] (assoc r c (r a)))
(defn seti [a _ c r] (assoc r c a))
(defn gtir [a b c r] (assoc r c (if (> a (r b)) 1 0)))
(defn gtri [a b c r] (assoc r c (if (> (r a) b) 1 0)))
(defn gtrr [a b c r] (assoc r c (if (> (r a) (r b)) 1 0)))
(defn eqir [a b c r] (assoc r c (if (= a (r b)) 1 0)))
(defn eqri [a b c r] (assoc r c (if (= (r a) b) 1 0)))
(defn eqrr [a b c r] (assoc r c (if (= (r a) (r b)) 1 0)))

(def instruction-set [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

; Evaluations
(defn behaves-like?
  "Test this opcode to see if it matches this sample. Return true or false."
  [{:keys [input output a b c]} opcode]
  (= output (opcode a b c input)))

(defn interesting?
  "For part 1, the sample is interesting if it behaves like at least n instructions.
  Lazily see if you can find 3"
  [n sample]
  (not (empty? (drop (dec n) (filter (partial behaves-like? sample) instruction-set)))))

(defn part1 [input]
  (count (filter (partial interesting? 3) (load-samples input))))

;; (part1 input-data)
;; => 500

;; Part 2: TODO