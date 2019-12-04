(ns adventofcode2019.day3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def input-data "resources/2019/day3-input.txt")

(def sample-data (seq ["R8,U5,L5,D3", "U7,R6,D4,L4"]))

(defn get-data
  "Read instructions from the file and return as a seq of 2 strings"
  [file]
  (with-open [rdr (clojure.java.io/reader "resources/2019/day3-input.txt")]
    (doall (line-seq rdr))))

(defn parse-move
  "Parse the move and return a vector with a x increment function y increment function
  and number of steps."
  [s]
  (let [[_ dir s] (re-matches #"([RULD])(\d+)" s)
        steps (Integer/parseInt s)]
    (condp = dir
      "R" [inc identity steps]
      "L" [dec identity steps]
      "U" [identity dec steps]
      "D" [identity inc steps]
      (throw (IllegalArgumentException. (format "Unrecognized direction - %s" dir))))))

(defn create-path
  "Given x and y coordinates and a move instruction, return a sequence of all
  [x,y] positions visited when executing that move, in order."
  [x y move]
  (let [[fx fy n] (parse-move move)]
    (map vector (take n (iterate fx (fx x))) (take n (iterate fy (fy y))))))

(defn track-path
  "Add coll to the map with coll items as keys, and an incrementing idx as value in
  such a way that repeat items retain the original idx value."
  [coll m idx]
  (reduce (fn [m [itm idx]]
            (assoc m itm (get m itm idx)))
          m
          (map vector coll (take (count coll) (iterate inc (inc idx))))))

(defn trace-path
  "Given a comma-separated string of moves, simulate the moves and return an ending
  position relative to the start, and set of all [dx,dy] coordinates visited."
  [instructions]
  (reduce (fn [{:keys [x y steps visited]} move]
            (let [path (create-path x y move)
                  [new-x new-y] (last path)
                  now-visited (track-path path visited steps)]
              {:x new-x :y new-y :steps (+ steps (count path)) :visited now-visited}))
          {:x 0 :y 0 :steps 0 :visited {}}
          (clojure.string/split instructions #",")))

(defn intersect-paths
  "Trace the paths of the two wires and return the relative intersection points"
  [[path1 path2]]
  (set/intersection (set (keys (:visited path1)))
                    (set (keys (:visited path2)))))

(defn closest-intersection
  "Return the intersection point at shortest manhattan distance."
  [wires]
  (reduce min (map (fn [[dx dy]] (+ (Math/abs dx) (Math/abs dy)))
                   (intersect-paths (map trace-path wires)))))

(defn part1 []
  (closest-intersection (get-data input-data)))

;; Part1:
; => 1519

(defn fastest-intersection
  "Return number of steps to the point of intersection with the fewest combined steps."
  [wires]
  (let [[path1 path2] (map trace-path wires)]
    (reduce (fn [answer point]
              (min answer (+ (get-in path1 [:visited point]) (get-in path2 [:visited point]))))
            Integer/MAX_VALUE
            (intersect-paths [path1 path2]))))

(defn part2 []
  (fastest-intersection (get-data input-data)))

;; Part2:
; => 14358
