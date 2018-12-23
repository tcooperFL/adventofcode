(ns adventofcode2018.day9
  (:require [util.trace :refer :all]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/9

(def score-period 23)

(defn create-game
  "Create a game already 2 staps in."
  [num-players]
  {:players (drop 1 (cycle (range 1 (inc num-players))))
   :scores  {}
   :circle  '(1 0)})

(defn advance
  "Play this next marble and return the new game state"
  [game current]
  (loop [player (first (:players game))
         circle (:circle game)]
    (assoc
      (if (zero? (rem current score-period))
        (let [[part1 [points & part2]] (split-at (- (count circle) 7) circle)]
          {:scores (update (:scores game) player (fnil + 0) current points)
           :circle  (concat part2 part1)})
        {:scores (:scores game)
         :circle (cons current (flatten (reverse (split-at 2 circle))))})
      :players (rest (:players game)))))

(defn play
  "Play all these marbles in a game and return the max score."
  [num-players num-marbles]
  (apply max
         (cons 0
               (vals (:scores (reduce advance
                                      (create-game num-players)
                                      (range 2 (inc num-marbles))))))))

;; Brute force simulation
(defn part1 []
  (play 425 70848))

; (part1)
; => 413188 (but it took far too long!)

; Alternate approach: zippers

(defn create-game2
  "Create a mutable data structure to hold before and after references"
  [num-players num-marbles]
  (let [z
        ; Preallocate the int arrays to avoid GC
        {:before  (int-array (inc num-marbles))
         :after   (int-array (inc num-marbles))
         :current (atom 0)
         :players (atom (cycle (range 1 (inc num-players))))
         :scores  (atom {})
         :size    (atom 1)}]
    (aset-int (:before z) 0 0)
    (aset-int (:after z) 0 0)
    z))

;; Readable accessors and mutators for the arrays.
(defn get-before [z i] (aget (:before z) i))
(defn set-before! [z i v] (aset-int (:before z) i v))
(defn get-after [z i] (aget (:after z) i))
(defn set-after! [z i v] (aset-int (:after z) i v))

(defn add!
  "Always add delta after the current, set current to that new location. Z is altered."
  [z value]
  (let [before (get-after z @(:current z))
        after (get-after z before)]
    ; Set before and after our new value
    (set-before! z value before)
    (set-after! z value after)
    ; Set before of the next and after of the previous to this value
    (set-before! z after value)
    (set-after! z before value)
    ; The value is the new current, and we've add one.
    (reset! (:current z) value)
    (swap! (:size z) inc)))

(defn remove!
  "Remove delta before this, and set current to the one after the removed one. Z is altered."
  [z delta]
  (let [target (nth (iterate (partial get-before z) @(:current z)) delta)
        before (aget (:before z) target)
        after (aget (:after z) target)]
    ; Orphan that target by bypassing it.
    (set-before! z after before)
    (set-after! z before after)
    ; Logically removed one.
    (swap! (:size z) dec)
    ; New current is the one after the one we removed.
    (reset! (:current z) after)
    target))

(defn display
  "Print a debug line as in the problem description"
  [z]
  (let [current @(:current z)
        player (first @(:players z))]
    (print (str "[" (if (zero? player) "-" (dec player)) "] "))
    (doseq [n (take @(:size z) (iterate (partial get-after z) 0))]
      (print (if (= n current) (str " (" n ")") (str " " n))))
    (println)))

(defn advance!
  "Place the next marble with this value"
  [game current]
  ;(display game)
  (if (zero? (rem current score-period))
    (swap! (:scores game)
           update (first @(:players game)) (fnil + 0) current (remove! game 7))
    (add! game current))
  (swap! (:players game) rest)
  nil)

(defn play2
  "Play all these marbles in a game and return the max score."
  [num-players num-marbles]
  (let [game (create-game2 num-players num-marbles)]
    (doseq [i (range 1 (inc num-marbles))]
      (advance! game i))
    (apply max (cons 0 (vals @(:scores game))))))

; Part 2
(defn part2 []
  (play2 425 7084800))

; (part2)
; => 3377272893

