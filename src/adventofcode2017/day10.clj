(ns adventofcode.day10)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/10
;; Part 1

(def input "157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30")
(def list-size 256)

; Generate a reversed replacement segment
(defn replacement [v from len]
  (reverse (take len (nthrest (cycle v) from))))

; Create [index value] pairs as update instructions for the array list
(defn overlay [v from len]
  (let [size (count v)]
    (partition 2
               (interleave
                 (map #(mod % size) (range from (+ from len)))
                 (replacement v from len)))))

; Mutate the array, overlaying a reversed segment starting at an offset
(defn rotate! [arr from len]
  (doseq [[i value] (overlay arr from len)]
    (aset arr i value)))

; Process all the lengths, each mutating a list, and return the resulting list
(defn process-rotations! [arr lengths cur skip]
  (reduce (fn [[cur skip] len]
            (rotate! arr cur len)
            [(+ cur len skip) (inc skip)])
          [cur skip]
          lengths))

; Process all the lengths, each mutating a list, and return the resulting list
(defn tie-knot [list-size lengths]
  (let [arr (int-array (range list-size))]
    (process-rotations! arr lengths 0 0)
    (seq arr)))

; Multiply the first two values together
(defn part1 [input]
  (let [lst (map read-string (clojure.string/split input #","))
        result (tie-knot list-size lst)]
    (* (first result) (second result))))

; => 62238

;; Part 2

; In part 2, length is input bytes padded with 5 more given values.
(defn lengths2 [input]
  (concat (seq (.getBytes input)) [17, 31, 73, 47, 23]))

; Run the rotation 64 times, preserving current, state, and array values.
(defn rotation64! [arr lengths]
  (loop [[cur skip] [0 0] n 64]
    (if (> n 0)
      (recur (process-rotations! arr lengths (mod cur list-size) skip) (dec n)))))

; Create the hex hash for the result of running 64 times.
(defn part2 [input]
  (let [arr (int-array (range list-size))]
    (rotation64! arr (lengths2 input))
    (->> arr
         (partition 16)
         (map #(reduce bit-xor %))
         (map #(.replace (format "%2s" (Integer/toHexString %)) " " "0"))
         (apply str))))

; (part2 input)
; => "2b0c9cc0449507a0db3babd57ad9e8d8"
