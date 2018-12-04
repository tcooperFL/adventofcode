(ns adventofcode.day9)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/9
;; Part 1

(def day9-input-file "resources/day9-input.txt")

; Fairly straight-forward state machine. Doesn't feel idiomatic.
(defn score-groups [s]
  (loop [score 0 open 0 pos 0 removed 0 state :process]
    (if (>= pos (count s))
      {:score score :removed removed}
      (let [c (.charAt s pos)]
        (if (= c \!)
          (recur score open (+ pos 2) removed state)
          (if (= state :garbage)
            (if (= c \>)
              (recur score open (inc pos) removed :process)
              (recur score open (inc pos) (inc removed) :garbage))
            (case c
              \< (recur score open (inc pos) removed :garbage)
              \{ (recur score (inc open) (inc pos) removed :process)
              \} (recur (+ score open) (dec open) (inc pos) removed :process)
              (recur score open (inc pos) removed :process))))))))

(defn part1 [input-file]
  (:score (score-groups (slurp day9-input-file))))

;; Part 2

(defn part2 [input-file]
  (:removed (score-groups (slurp day9-input-file))))