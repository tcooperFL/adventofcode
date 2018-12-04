(ns adventofcode2017.day11)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/11
;; Part 1

(def day11-input-file "resources/2017/day11-input.txt")

(def coords {"n" [0 1], "ne" [1 1], "se" [1 -1], "s" [0 -1], "sw" [-1 -1], "nw" [-1 1]})

; Follow the path and return the [x,y] of the final location.
(defn locate [path]
  (->>
    (clojure.string/split path #",|\n")
    (map coords)
    (reduce (fn [{:keys [loc max-abs-x max-abs-y]} [dx dy]]
              (let [new-x (+ (loc 0) dx)
                    new-y (+ (loc 1) dy)]
                {:loc       [new-x new-y]
                 :max-abs-x (max max-abs-x (Math/abs ^long new-x))
                 :max-abs-y (max max-abs-y (Math/abs ^long new-y))}))
            {:loc [0 0] :max-abs-x 0 :max-abs-y 0})))

; Returning to [0,0] takes abs(x) or abs(y), whichever is greater
(defn part1 [input-file]
  (apply max (map #(Math/abs ^long %) (:loc (locate (slurp input-file))))))

; (part1 day11-input-file)
; => 761

;; Part 2
(defn part2 [input-file]
  (let [{:keys [max-abs-x max-abs-y]} (locate (slurp input-file))]
    (max max-abs-x max-abs-y)))

; (part2 day11-input-file)
; => 1542

