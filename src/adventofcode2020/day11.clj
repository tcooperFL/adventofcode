(ns adventofcode2020.day11)

(def input-data "resources/2020/day11-input.txt")

(defn get-data
  "Get the data as an array (rows) of arrays (seats)"
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (into [] (map vec) (line-seq rdr))))

(defn inspect [[m r c]] (get-in m [r c]))

(defn occupied? [p] (= (inspect p) \#))

(defn surrounding [[m r c]]
  (map (fn [[dx dy]] [m (+ r dx) (+ c dy)])
       (for [dx [-1 0 1] dy [-1 0 1] :when (not= [0 0] [dx dy])] [dx dy])))

(defn occupied-adjacent [p]
  (count (filter occupied? (surrounding p))))

(defn tick
  "Create the next state for this position"
  [p]
  (condp = (inspect p)
    \L (if (zero? (occupied-adjacent p)) \# \L)
    \# (if (>= (occupied-adjacent p) 4) \L \#)
    (inspect p)))

(defn round
  "Create the next round of the seating state"
  [m]
  (mapv (fn [r]
         (mapv (fn [c] (tick [m r c]))
              (range (count (first m)))))
       (range (count m))))

(defn total-occupied [m]
  (count (filter #{\#} (flatten m))))

(defn solve1 [m]
  (total-occupied (ffirst (drop-while (fn [[m1 m2]] (not= m1 m2))
                                      (partition 2 1 (iterate round m))))))

(defn part1 [file]
  (solve1 (get-data file)))

; (time (part1 input-data))
; "Elapsed time: 3832.046739 msecs"
; => 2344


