(ns adventofcode2018.day6)

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/6

(def input-data "resources/2018/day6-input.txt")

(defn get-data
  "Parse the input file into [x y] coordinates"
  []
  (map vec (partition 2 (read-string (str "[" (slurp input-data) "]")))))

(defn bounding-box
  "What is the smallest bounding box that contains all these points"
  [coords]
  (list
    [(apply min (map first coords)) (apply min (map second coords))]
    [(apply max (map first coords)) (apply max (map second coords))]))

(defn distance
  "Calculate the Manhattan distance between these two points"
  [c1 c2]
  (apply + (mapv #(Math/abs (- %1 %2)) c1 c2)))

(defn create-grid
  "Create a grid of locations the size of the bounding box around these coordinates"
  [coords]
  (let [[b0 b1] (bounding-box coords)]
    (for [x (range (first b0) (inc (first b1)))
          y (range (second b0) (inc (second b1)))]
      [x y])))

(defn compute-closest-coord
  "Given a set of coordinates, compute which is closest to this location.
  If it is a tie, return nil."
  [coords location]
  (let [[closest _]
        (reduce (fn [[closest-coords closest-dist :as closest] coord]
                  (let [dist (distance location coord)]
                    [(cond (= dist closest-dist) (conj closest-coords coord)
                           (< dist closest-dist) #{coord}
                           :default closest-coords)
                     (min closest-dist dist)]))
                [#{} Integer/MAX_VALUE]
                coords)]
    (if (= (count closest) 1)
      (first closest))))

(defn map-closest-coord
  "Associate with each grid point the closest point"
  [coords grid]
  (pmap #(hash-map :location % :closest (compute-closest-coord coords %)) grid))

(defn edge-coords
  "Return the coords that are closest along the edges of the bounding box"
  [coords mapped]
  (let [[[x1 y1] [x2 y2]] (bounding-box coords)
        x-edges #{x1 x2}
        y-edges #{y1 y2}]
    (reduce (fn [a {:keys [location closest]}]
              (if (and closest
                       (or (x-edges (first location))
                           (y-edges (second location))))
                (conj a closest)
                a))
            #{}
            mapped)))

(defn prune-infinites
  "Eliminate all calculations on the perimeter because they will be infinite"
  [coords mapped]
  (let [invalid-closest (conj (edge-coords coords mapped) nil)]
    (remove #(contains? invalid-closest (:closest %)) mapped)))

(defn part1
  "Return the size of the largest non-infinite region"
  [coords]
  (let [mapped (map-closest-coord coords (create-grid coords))]
    (->>
      (prune-infinites coords mapped)
      (map :closest)
      frequencies
      vals
      (apply max))))

; (part1 (get-data))
; => 4976

(defn compute-total-distances
  "Compute the sum of the distances from all coords to this location"
  [coords location]
  (reduce + (map (partial distance location) coords)))

(defn part2
  "Return the size of the region containing all locations which have a total distance < 10000"
  [coords]
  (count
    (filter #(< % 10000)
            (pmap (partial compute-total-distances coords) (create-grid coords)))))

; (part2 (get-data))
; -> 46462

