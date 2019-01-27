(ns adventofcode2018.day18
  (:require [clojure.java.io :as io]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/18

(def input-data "resources/2018/day18-input.txt")
(def sample-data "resources/2018/day18-sample.txt")

(def decode {:open \. :trees \| :lumberyard \#})

;; Building

(defn load-acres
  "Parse the input file to return a landscape"
  [file]
  (let [encode (into {} (mapv (comp vec reverse) (map vec decode)))]
    (with-open [rdr (io/reader file)]
      (into {} (for [[y line] (map-indexed vector (line-seq rdr))
                     [x acre] (map-indexed vector (seq line))]
                 [[x y] (encode acre)])))))

(defn build-landscape
  "Build the landscape from the map of acres"
  [acres]
  {:acres acres
   :max-x (apply max (map first (keys acres)))
   :max-y (apply max (map second (keys acres)))
   :time  0})

;; Debugging

(defn render
  "Render the landscape as illustrated in the problem statement"
  [{:keys [acres max-x max-y time]}]
  (println "\nAfter" time "minutes:")
  (doseq [y (range 0 (inc max-y))]
    (doseq [x (range 0 (inc max-x))]
      (print (decode (acres [x y]))))
    (println)))

;; Part1

(defn neighbors
  "Return a map of each type and count of my neighbors"
  [acres [x y]]
  (->>
    (for [dx [-1 0 1] dy [-1 0 1] :when (not= dx dy 0)] [(+ x dx) (+ y dy)])
    (map acres)
    (remove nil?)
    frequencies))

(defn change [content neighbors]
  (case content
    :open (if (>= (get neighbors :trees 0) 3) :trees :open)
    :trees (if (>= (get neighbors :lumberyard 0) 3) :lumberyard :trees)
    :lumberyard
      (if (and (>= (get neighbors :lumberyard 0) 1)
               (>= (get neighbors :trees 0) 1))
        :lumberyard
        :open)))

(defn tick
  "Given a landscape, return its state after one minute."
  [{:keys [acres time] :as landscape}]
  (assoc landscape
    :time (inc time)
    :acres (reduce
             (fn [m [pos content]]
               (assoc m pos (change content (neighbors acres pos))))
             {}
             acres)))

(defn age
  "Simulate aging of the landscape for this many minutes."
  [landscape n]
  (first (drop n (iterate tick landscape))))

(defn score
  "Multiple number of wooded acres by number of lumberyards"
  [landscape]
  (let [contents (vals (:acres landscape))]
    (* (count (filter #{:trees} contents))
       (count (filter #{:lumberyard} contents)))))

(defn part1 [input]
  (let [result (age (build-landscape (load-acres input)) 10)]
    (score result)))

;; (part1 input-data)
;; => 583426

;; Part 2:

(def epoch 1000000000)

(defn find-loop
  "Find when the state is repeated"
  [landscape]
  (loop [seen {} state landscape]
    (let [next (tick state)]
      (if (contains? seen (:acres next))
        [(get seen (:acres next)) next]
        (recur (assoc seen (:acres next) (:time next)) next)))))

(defn part2
  "Look for a loop, and then do the math."
  ([input] (part2 (build-landscape (load-acres input)) epoch))
  ([landscape gens]
   (let [[loop-start end-state] (find-loop landscape)
         loop-size (- (:time end-state) loop-start)
         remaining (rem (- gens loop-start) loop-size)]
     (println loop-size remaining)
     (score (age end-state remaining)))))

;; (part2 input-data)
;; => 169024


