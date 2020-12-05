(ns adventofcode2020.day5
  (:require [clojure.java.io :as io]
            [util.trace :as t]))

(def input-data "resources/2020/day5-input.txt")
(def plane {:rows 128 :columns 8})

(defn process-file [t file]
  (with-open [rdr (clojure.java.io/reader file)]
    (into [] t (line-seq rdr))))

(defn first-half [[low high]]
  [low (+ low (quot (- high low) 2))])

(defn last-half [[low high]]
  [(+ low (quot (- high low) 2)) high])

(defn locate [s]
  (reduce (fn [{:keys [r c] :as m} dir]
            (case dir
              \F (assoc m :r (first-half r))
              \B (assoc m :r (last-half r))
              \L (assoc m :c (first-half c))
              \R (assoc m :c (last-half c))
              m))
          {:r [0 (dec (:rows plane))]
           :c [0 (dec (:columns plane))]}
          (seq s)))

(defn create-pass [row col]
  {:row    row
   :col    col
   :seatId (+ (* 8 row) col)})

(defn boarding-pass [s]
  (let [{[_ row] :r [_ col] :c} (locate s)]
    (create-pass row col)))

(defn part1 [file]
  (apply max
         (map :seatId
              (process-file (map boarding-pass) file))))
;;=> 994

(def possible-seats
  (for [r (range 1 (dec (:rows plane))) c (range (:columns plane))]
    (create-pass r c)))

(defn part2 [file]
  (let [passes (process-file (map boarding-pass) file)
        candidates (reduce disj (set possible-seats) passes)
        taken-seats (set (map :seatId passes))]
    ; See which candidate seat has a taken seat +1 and -1 the seatId
    (some #(and (contains? taken-seats (inc (:seatId %)))
                (contains? taken-seats (dec (:seatId %)))
                %)
          candidates)))