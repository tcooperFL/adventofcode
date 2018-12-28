(ns adventofcode2018.day11)

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/11

(defn create-grid
  "Create a 300x300 grid of power levels"
  [size snum]
  (letfn [(starting-power-level [[x y :as p]]
            (let [rackID (+ x 10)]
              (* rackID (+ snum (* rackID y)))))
          (hundredth-digit [x] (rem (quot x 100) 10))
          (grid-value [p] (- (hundredth-digit (starting-power-level p)) 5))]
    (let [grid (make-array Integer/TYPE size size)]
      (doseq [x (range size)]
        (doseq [y (range size)]
          (aset-int grid x y (grid-value [(inc x) (inc y)]))))
      grid)))

(defn power-level
  "Return the power-level of this point in the grid."
  [g [x y]]
  (aget g (dec x) (dec y)))

(def sum-square
  "Compute the sum of the square given lowest x and y point and size in either direction.
   Then memorize it."
  (memoize
    (fn [g [x y :as p] size]
      (if (= size 1)
        (power-level g p)
        (+ (sum-square g [(inc x) (inc y)] (dec size))
           (power-level g [x y])
           (reduce + (for [i (range (inc x) (+ x size))] (power-level g [i y])))
           (reduce + (for [j (range (inc y) (+ y size))] (power-level g [x j]))))))))

(defn larger-score
  "Given 2 maps, return the one with the greater :score."
  [largest contender]
  (if (> (:score contender) (:score largest)) contender largest))

(defn largest-square
  "Find the largest square of a given size larger then the given value.
   If value isn't supplied, minimum int is assumed.
   If size isn't supplied, find largest square of ANY size."
  ([g]
    ; We can start at 3x3 since we know the largest 3x3 is 30 (from above) and
    ; the largest 2x2 can only be 16. Use all CPU cores to do work in parallel.
   (reduce larger-score
           (pmap (partial largest-square g) (range 3 (inc (count g))))))
  ([g size]
   (reduce larger-score
           {:score Integer/MIN_VALUE}
           (map #(hash-map :point % :score (sum-square g % size) :size size)
                (for [x (range 1 (- (inc (count g)) size))
                      y (range 1 (- (inc (count g)) size))]
                  [x y])))))

(defn part1 [snum]
  (largest-square (create-grid 300 snum) 3))

; (part1 6042)
; => {:size 3, :score 30, :point [21 61]}

; Part 2
(defn part2 [snum]
  (largest-square (create-grid 300 snum)))

; (part2 6042)
; => {:size 12, :score 119, :point [232 251]}

