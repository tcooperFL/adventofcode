(ns adventofcode2017.day17)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/17
;; Part 1

;; Problem setup data
(def steps 312)

(defn spin-loop
  "Spin by step until you reach the limit, then display the next few"
  [step limit]
  (loop [iteration 1 current 0 buffer '(0)]
    (if (> iteration limit)
      {:current-position current
       :next-few         (take 3 (drop current buffer))}
      (let [new-cur (inc (mod (+ current step) (count buffer)))]
        (recur (inc iteration)
               new-cur
               (concat (take new-cur buffer)
                       (list iteration)
                       (nthrest buffer new-cur)))))))

(defn part1 []
  (spin-loop steps 2017))

; (part1)
; => {:current-position 529, :next-few (2017 772 1941)}

;; Part 2
;; Position 2 changes every time the spin wraps around, when the mod = 0

(defn spin-loop-head
  "Spin by step until you reach the limit, then display the second element"
  [step limit]
  (loop [iter 1 len 1 cur 0 answer 0]
    (if (>= iter limit)
      answer
      (let [next-cur (inc (mod (+ cur step) len))]
        (recur (inc iter) (inc len) next-cur (if (= 1 next-cur) iter answer))))))

(defn part2 []
  (spin-loop-head steps 50000000))

;(time (spin-loop-head 312 50000000))
;"Elapsed time: 1296.521537 msecs"
;=> 42729050
