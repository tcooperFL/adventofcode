(ns adventofcode2017.day3)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/3
;; Part 1

; My approach here is to directly calculate and not search.

(defn i-to-d
  "Given the number i to find, return the orthogonal distance d from center, aka the ring number"
  [i]
  (let [x (int (Math/ceil (Math/sqrt i)))
        r (if (even? x) (+ x 1) x)]
    (/ (- r 1) 2)))

(defn ring-start
  "For a given distance, what is the lowest number on that ring"
  [d]
  (let [x (- (* 2 d) 1)]
    (+ 1 (* x x))))

(defn ring-end
  "For a given distance, what is the largest number on that ring"
  [d]
  (- (ring-start (+ 1 d)) 1))

(defn ring-size [d] (inc (- (ring-end d) (ring-start d))))

(defn dist-to-orthogonal
  "Distance on the ring from i to a horizontal or vertical path to center"
  [i]
  (if (<= i 1)
    0
    (let [d (i-to-d i)
          side-size (/ (+ 1 (ring-size d)) 4)
          offset (rem (- i d 1) side-size)]
      (min offset (- side-size offset)))))

(defn dist-to-center
  "Manhattan distance to center"
  [i]
  (+ (dist-to-orthogonal i) (i-to-d i)))

; (dist-to-center 368078)


;; Part 2

; Using infinite and recursively defined sequences to build an infinite sequence
; of [x,y] coordinates that follow the path from center, then use these to create
; keys in a map to store values as I compute them.

; Generate the sequence of coordinates in 1 dimension spiraling out
(defn deltas [cy]
  (mapcat #(repeat (Math/floor (/ %1 2)) %2) (range) cy))

; X and Y coordinates deltas offset by 90 degrees
(def x-deltas (deltas (cycle [-1 0 1 0])))
(def y-deltas (deltas (cycle [0 -1 0 1])))

; Create x and y actuals by applying deltas
(def x-coords (cons 0 (lazy-seq (map + x-coords x-deltas))))
(def y-coords (cons 0 (lazy-seq (map + y-coords y-deltas))))

; Infinite sequence of all cell coordinates spiraling out from center
(def coords (map vector x-coords y-coords))

; Sum of all neighbors already computed
(defn sum-of-neighbors [m [x y]]
  (apply + (for [dx [0 1 -1] dy [0 1 -1]]
             (get m [(+ x dx) (+ y dy)] 0))))

; Traverse the spiral, computing results util you reach a given limit.
; Return the next computed value greater than that limit.
(defn part2 [limit]
  (loop [computed (transient {[0,0] 1})
         coordinates coords]
    (let [coord (first coordinates)
          value (sum-of-neighbors computed coord)]
      (if (<= value limit)
        (recur (assoc! computed coord value) (rest coordinates))
        {coord value}))))

; (part2 368078)
