(ns adventofcode2018.day17
  (:require [clojure.java.io :as io]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/17

(def input-data "resources/2018/day17-input.txt")
(def sample-data "resources/2018/day17-sample.txt")

(def display-chars {:sand \. :clay \# :water \~ :flow \| :spring \+})

(def spring-coords [500 0])

;; Building

(defn build-vein
  "Build clay walls to merge into the ground slice"
  [s]
  (let [[_ c1 d1 _ d21 d2n] (re-find #"(\w)=(\d+), (\w)=(\d+)..(\d+)" s)
        v1 (Integer/parseInt d1)
        span (range (Integer/parseInt d21) (inc (Integer/parseInt d2n)))
        coord (fn [v1 v2] (if (= c1 "x") [v1 v2] [v2 v1]))]
    (reduce #(assoc %1 (coord v1 %2) :clay) {} span)))

(defn load-clay
  "Parse the input file to return a 2D vertical slice showing the clay"
  [file]
  (with-open [rdr (io/reader file)]
    (reduce #(merge %1 (build-vein %2)) {} (line-seq rdr))))

(defn size-grid
  "Return the high and low x, and high y for this grid"
  [[[[x y] _] & _ :as m]]
  (reduce (fn [[x1 y1 x2 y2] [x y]]
            [(min x x1) (min y y1) (max x x2) (max y y2)])
          [x y x y]
          (keys m)))

(defn build-slice
  "Build the initial state of the slice"
  [input]
  (let [tiles (load-clay input)
        [low-x low-y high-x high-y] (size-grid tiles)]
    {:tiles   (assoc tiles spring-coords :spring)
     :x-range [low-x high-x]
     :min-y   low-y
     :max-y   high-y}))

(defn material-at
  "Return the material at this position in the slice"
  [slice coords]
  (get (:tiles slice) coords :sand))

;; Debugging

(defn render
  "Render the slice as illustrated in the problem statement"
  [{:keys [x-range max-y] :as slice}]
  (let [arg1 (str "%" (count (str max-y)) "d ")]
    (doseq [y (range 0 (inc max-y))]
      ; (print (format arg1 y))
      (doseq [x (range (dec (first x-range)) (+ (second x-range) 2))]
        (print (display-chars (material-at slice [x y]))))
      (println))))

;; Part1

(defn count-tiles
  "Count the number of tiles that have water or were on the path"
  [pred slice]
  (->> (:tiles slice)
       (filter (fn [[[_ y] _]] (>= y (:min-y slice))))
       (map second)
       (filter pred)
       count))

(defn down
  "Search downward until you hit something or bottom out. Return the highest y before you hit it."
  [slice [x y]]
  (some #(if (or (not= :sand (material-at slice [x (inc %)]))
                 (= % (:max-y slice)))
           %)
        (range y (inc (:max-y slice)))))

(defn find-end
  "From this point, move by the inc-fn looking for clay or an opportunity to drop"
  [slice [x y] inc-fn]
  (first (drop-while #(not (or (= :clay (material-at slice [(inc-fn %) y]))
                               (and (#{:sand :flow} (material-at slice [(inc-fn %) y]))
                                    (#{:sand :flow} (material-at slice [% (inc y)])))))
                     (iterate inc-fn x))))

(defn fill-with
  "Fill these positions in the slice with this material."
  [material slice x1 x2 y1 y2]
  (update slice
          :tiles merge (into {}
                             (for [x (range x1 (inc x2)) y (range y1 (inc y2))]
                               [[x y] material]))))

(defn fill-bucket
  "Given this position in a slice, return the slice with the bucket filled and record the top"
  [base-slice [x base-y]]
  (loop [y base-y
         slice base-slice]
    (let [lx (find-end slice [x y] dec)
          rx (find-end slice [x y] inc)]
      (if (= :clay (material-at slice [(dec lx) y]) (material-at slice [(inc rx) y]))
        (recur (dec y) (fill-with :water slice lx rx y y))
        (assoc slice :top [x y])))))

(defn below [[x y]] [x (inc y)])

(declare pour)

(defn overflow-bucket
  "Fill bucket and then overflow new pours."
  [slice pos]
  (let [filled-slice (fill-bucket slice pos)
        [_ top-y :as top] (:top filled-slice)
        lx (find-end filled-slice top dec)
        rx (find-end filled-slice top inc)
        overflowed-slice (fill-with :flow filled-slice lx rx top-y top-y)]
    (pour (pour overflowed-slice [lx top-y]) [rx top-y])))

(defn pour
  "Simulate pouring water into the slice until it can take no more"
  ([slice] (pour slice (below spring-coords)))
  ([base-slice [x y :as start-pos]]
   (if (= start-pos (:top base-slice))
     base-slice
     ; First pour down if you can. If not just return the slice.
     (if-let [bottom-y (down base-slice start-pos)]
       (let [slice (fill-with :flow base-slice x x y bottom-y)]
         ; If you bottomed out, just record the flow and return the slice.
         (if (= :water (material-at slice [x (inc bottom-y)]))
           (overflow-bucket slice [x bottom-y])
           (if (not= :clay (material-at slice [x (inc bottom-y)]))
             slice
             ; Else fill the bucket to the top and recur
             (overflow-bucket slice [x bottom-y]))))
       base-slice))))

(defn part1
  "Keep adding water until it is full, then count how many squares water reached."
  [input]
  (count-tiles #{:water :flow} (pour (build-slice input))))

;; (part1 input-data)
;; => 27206

;; Part 2:
(defn part2
  "Count only standing water"
  [input]
  (count-tiles #{:water} (pour (build-slice input))))

;; (part2 input-data)
;; => 21787
