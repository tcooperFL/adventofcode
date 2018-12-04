(ns adventofcode2017.day16)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/16
;; Part 1

;; Problem setup data
(def day16-input-file "resources/2017/day16-input.txt")
(def line-size 16)

(defn alphabet-string
  "Create a string of n alphabet chars starting with 'a'"
  [n]
  (take n (map (comp str char) (iterate inc (int \a)))))

; Our dancers in initial positions.
(def dance-line (zipmap (range) (alphabet-string line-size)))

(defn line-to-string
  "String form of the dance line in order"
  [line]
  (apply str (mapcat #(line %) (range (count line)))))

;; Instruction implementations
(defn spin
  "Everyone shift to the right x positions, wrapping around to the front"
  [line-map x _]
  (let [len (count line-map)]
    (reduce (fn [m [i v]] (assoc m (mod (+ i x) len) v)) {} line-map)))

(defn partner
  "Implement the partner move, where two named partners swap positions"
  [line-map a b]
  (let [akey (some (fn [[k v]] (if (= v a) k)) line-map)
        bkey (some (fn [[k v]] (if (= v b) k)) line-map)]
    (assoc line-map akey b bkey a)))

(defn exchange
  "Dancers at positions x and y change places."
  [line-map x y]
  (assoc line-map x (line-map y) y (line-map x)))

; Mapping an instruction to function and arguments
(def instructions
  {"s" [spin #(Long/parseLong %) (constantly nil)]
   "x" [exchange #(Long/parseLong %) #(Long/parseLong %)]
   "p" [partner identity identity]
   })

(defn create-moves
  "Given the program description s, return a sequence of executable moves."
  [s]
  (map (fn [[matched move arg1 arg2]]
         (let [[fn arg1-parser arg2-parser] (get instructions move)]
           (list matched fn (arg1-parser arg1) (arg2-parser arg2))))
       (re-seq #"(s|x|p)([^/,]+)/?([^,\s]*)" s)))

(defn dance
  "Apply the dance moves to yield dancers in new positions."
  [moves line]
  (reduce (fn [d [s f a1 a2]] (f d a1 a2)) line moves))

(defn dance-forever
  "Returns an infinite lazy sequence of the dance line after 1..n dances"
  [moves]
  (map line-to-string (iterate (partial dance moves) dance-line)))

;; Solutions
(defn part1
  "Return the order of the dancers after dancing the moves just once."
  [input-file]
  (second (dance-forever (create-moves (slurp input-file)))))

(defn find-repeats
  "Search the sequence and find the [pos line] of the first repeat of line."
  [dances]
  (let [start (first dances)]
    (first (drop-while #(not= start (second %))
                       (drop 1 (map-indexed vector dances))))))

(defn part2
  "Return the order of the dancers after dancing the moves 1 billion times"
  [input-file]
  (let [dances (dance-forever (create-moves (slurp input-file)))
        cycle-size (first (find-repeats dances))
        after-repeats (mod 1000000000 cycle-size)]
    (first (drop after-repeats dances))))

; (part1 day16-input-file)
; => bijankplfgmeodhc

; (part2 day16-input-file 1000000000
; => bpjahknliomefdgc

; Debugging
; (def dances (dance-forever (create-moves (slurp day16-input-file))))
