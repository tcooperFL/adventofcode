(ns adventofcode2018.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/12

(def input-data "resources/2018/day12-input.txt")
(def sample-data "resources/2018/day12-sample.txt")

;; Very straight-forward idiomatic solution for part 1
(def initial-pattern #"initial state: (.+)")
(def rule-pattern #"(.....) => (.)")
(def rule-size 5)
(def padding (take (dec rule-size) (cycle '(\space))))

(defn normalize-state [s] (s/replace s #"\." " "))

(defn create-puzzle
  "Parse the input file to yield an initial state and rule set."
  [file]
  (with-open [rdr (io/reader file)]
    (let [lines (line-seq rdr)
          initial-state (second (re-matches initial-pattern (first lines)))]
      {:state      (normalize-state initial-state)
       :start      0                                        ; Track the index of the first pot
       :generation 0
       :rules      (reduce (fn [m [_ pattern result]]
                             (assoc m (seq (normalize-state pattern))
                                      (first (normalize-state result))))
                           {}
                           (map (partial re-matches rule-pattern)
                                (drop 2 lines)))})))

(defn explode
  "Explode the string, adding padding on both ends to allow partitioning"
  [s]
  (concat padding (seq s) padding))

(defn next-generation
  "Given a puzzle, compute the next generation, tracking the first pot."
  [puzzle]
  (let [new-state (apply str
                         (map #(get (:rules puzzle) % \space)
                              (partition rule-size 1 padding
                                         (explode (:state puzzle)))))]
    (assoc puzzle
      :state (s/trim new-state)
      :generation (inc (:generation puzzle))
      :start (+ (- (:start puzzle) (int (/ (dec rule-size) 2))) (s/index-of new-state \#)))))

(defn score
  "Score this puzzle as the sum of the indexes that have pots"
  [p]
  (->> (map vector (range (:start p) Long/MAX_VALUE) (:state p))
       (filter #(= (second %) \#))
       (map first)
       (reduce +)))

(defn part1 [puzzle gens]
  (score (first (drop gens (iterate next-generation puzzle)))))

;; (part1 (get-data input-data) 20)
;; => 2140

;; Part 2
; Straight-forward won't work with 50 billion generations!
(def generations 50000000000)

(defn find-loop
  "Find when a state string is repeated"
  [puzzle]
  (loop [seen {} p puzzle]
    (let [next (next-generation p)]
      (if (contains? seen (:state next))
        [(get seen (:state next)) next]
        (recur (assoc seen (:state next) next) next)))))

(defn part2
  "Look for a loop, and then do the math."
  [puzzle gens]
  (let [[start end] (find-loop puzzle)
        loop-size (- (:generation end) (:generation start))
        shift-per-loop (- (:start end) (:start start))
        remaining (- gens (:generation start))
        loop-shift (* (quot remaining loop-size) shift-per-loop)
        offset (rem remaining loop-size)
        base (first (drop offset (iterate next-generation start)))]
    (score (update base :start + loop-shift))))

;; => 1900000000384
