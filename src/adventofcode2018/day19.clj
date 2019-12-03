(ns adventofcode2018.day19
  (:require [clojure.java.io :as io])
  (:use util.trace
        clojure.repl
        clojure.pprint))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/19

(def input-data "resources/2018/day19-input.txt")
(def sample-data "resources/2018/day19-sample.txt")

(def instruction-set
  [:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr])


; Opcodes
; Operations
(defn addr [a b c r] (aset r c (+ (aget r a) (aget r b))))
(defn addi [a b c r] (aset r c (+ (aget r a) b)))
(defn mulr [a b c r] (aset r c (* (aget r a) (aget r b))))
(defn muli [a b c r] (aset r c (* (aget r a) b)))
(defn banr [a b c r] (aset r c (bit-and (aget r a) (aget r b))))
(defn bani [a b c r] (aset r c (bit-and (aget r a) b)))
(defn borr [a b c r] (aset r c (bit-or (aget r a) (aget r b))))
(defn bori [a b c r] (aset r c (bit-or (aget r a) b)))
(defn setr [a _ c r] (aset r c (aget r a)))
(defn seti [a _ c r] (aset r c a))
(defn gtir [a b c r] (aset r c (if (> a (aget r b)) 1 0)))
(defn gtri [a b c r] (aset r c (if (> (aget r a) b) 1 0)))
(defn gtrr [a b c r] (aset r c (if (> (aget r a) (aget r b)) 1 0)))
(defn eqir [a b c r] (aset r c (if (= a (aget r b)) 1 0)))
(defn eqri [a b c r] (aset r c (if (= (aget r a) b) 1 0)))
(defn eqrr [a b c r] (aset r c (if (= (aget r a) (aget r b)) 1 0)))


;; Parsing lines
(defn parse-ip [s] (read-string (re-find #"\d+" s)))
(defn parse-instruction [s]
  (let [[_ name a b c] (re-find #"(\w+) (\d+) (\d+) (\d+)" s)]
    {:opcode name :fn (resolve (symbol name)) :a (read-string a) :b (read-string b) :c (read-string c)}))

(defn load-program
  "Load the set of instructions from this file, bypassing sample cases."
  [file]
  (with-open [rdr (io/reader file)]
    (let [lines (line-seq rdr)]
      {:ip-register  (parse-ip (first lines))
       :instructions (mapv parse-instruction (rest lines))})))

(defn exec!
  "Execute this instruction, returning the resulting registers, or nil if there is an error."
  [{:keys [opcode fn a b c]} r]
  (try
    (fn a b c r)
    (catch Exception e
      (println opcode a b c r "caused exception" (.toString e))
      nil)))

;; Running

(defn create-process [program] () 6 0
  (assoc program :pc 0 :ip-value 0 :registers (int-array 6)
                 #_(into {} (map-indexed vector (repeat 6 0)))))

(defn run-process!
  "Execute one instruction in the given process"
  [{:keys [pc ip-register ip-value instructions registers] :as program}]
  (let [program-size (count instructions)]
    (loop [ip (aget registers ip-register)]
      (aset registers ip-register ip)
      (let [instruction (instructions ip)]
        #_ (printf "ip=%d %s %s %d %d %d"
                ip (str (vec registers))
                (:opcode instruction)
                (:a instruction)
                (:b instruction)
                (:c instruction))
        (exec! instruction registers)
        (let [next-ip (inc (aget registers ip-register))]
          #_(printf " %s\n" (str (vec registers)))
          (if (< next-ip program-size)
            (recur next-ip)))))))

(defn part1 [input]
  (let [p (create-process (load-program input))]
    (run-process! p)
    (aget (:registers p) 0)))

;; (part1 input-data)
;; => 993

;; Part2

(defn part2 [input]
  (let [p (create-process (load-program input))]
    (aset (:registers p) 0 1)
    (run-process! p)
    (aget (:registers p) 0)))

;; (part2 input-data)
;; => 533
