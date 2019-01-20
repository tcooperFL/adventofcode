(ns adventofcode2018.day16
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

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

(defn exec
  "Execute this instruction, returning the resulting registers, or nil if there is an error."
  [opcode a b c r]
  (try
    ((resolve (symbol (name opcode))) a b c r)
    (catch Exception e
      (println (symbol (name opcode)) a b c r "caused exception" (.toString e))
      nil)))

(def instruction-set
  [:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr])

; Evaluations
(defn behaves-like?
  "Test this opcode to see if it matches this sample. Return true or false."
  [{:keys [input output a b c]} opcode]
  (= output (exec opcode a b c input)))

(defn interesting?
  "For part 1, the sample is interesting if it behaves like at least n instructions.
  Lazily see if you can find 3"
  [n sample]
  (not (empty? (drop (dec n) (filter (partial behaves-like? sample) instruction-set)))))

(defn part1 [input]
  (count (filter (partial interesting? 3) (load-samples input))))

;; (part1 input-data)
;; => 500

;; Part 2: Run the program

(defn skip-samples
  "Read lines until you have read 3 consecutive empty lines, return the rest"
  [lines]
  (loop [[line & tail] lines
         blanks 0]
    (if (and (= blanks 2) (empty? line))
      tail
      (recur tail (if (empty? line) (inc blanks) 0)))))

(defn load-program
  "Load the set of instructions from this file, bypassing sample cases."
  [file]
  (with-open [rdr (io/reader file)]
    (reduce conj [] (map parse-instruction (skip-samples (line-seq rdr))))))

(defn opcode-mappings
  "For each opcode, find the set of instructions that behave like all samples for that opcode"
  [samples]
  (reduce (fn [m {:keys [code] :as sample}]
            (assoc m code (set (filter (partial behaves-like? sample) (m code)))))
          (into {} (take (count instruction-set)
                         (map-indexed vector (repeat (set instruction-set)))))
          samples))

(defn opcode-numbers
  "For all opcode numbers, collect the set of opcodes that satisfied the samples for that number"
  [samples]
  (reduce (fn [m [num codes]]
            (reduce #(update %1 %2 (fnil conj #{}) num) m codes))
          {} (opcode-mappings samples)))

(defn assign-opcode
  "Assign this num to this opcode, and eliminate that opcode from all other number choices"
  [num opcode mappings numbers]
  (assoc
    (reduce (fn [m n] (update m n disj opcode)) mappings (numbers opcode))
    num opcode))

(defn reduce-opcodes
  "Reduce opcode choices based on cascading elimination when one is the only choice."
  [samples]
  {:post [(not-any? #(coll? (second %)) %)]}
  (let [possibilities (opcode-numbers samples)]
    (loop [mappings (opcode-mappings samples)]
      (if-let [singles (seq (filter (fn [[_ v]] (and (coll? v) (= (count v) 1))) mappings))]
        (recur
          (reduce (fn [m [n codes]]
                    (assign-opcode n (first codes) m possibilities))
                  mappings singles))
        mappings))))

(defn running? [process] (not (contains? process :completion)))

(defn step
  "Execute the next instruction and advance the state accordingly."
  [{[[opcode-num a b c] & next-instructions] :instructions
    opcodes                                  :opcodes
    registers                                :registers
    :as                                      process}]
  (if (nil? opcode-num)
    (assoc process :completion :success)
    (let [opcode (opcodes opcode-num)]
      (if-let [result (exec opcode a b c (or registers [0 0 0 0]))]
        (assoc process
          :instructions next-instructions
          :opcodes (assoc opcodes opcode-num opcode)
          :registers result
          :pc (inc (get process :pc 0)))
        (assoc process :completion :failure)))))

(defn part2 [input]
  ; Use part1 code to create full starting mapping to create process and run it.
  (->> {:opcodes (reduce-opcodes (load-samples input))
        :instructions (load-program input)}
       (iterate step)
       (drop-while running?)
       first
       :registers
       first))

;; Part2
;; (part2 input-data)
;; => 533
