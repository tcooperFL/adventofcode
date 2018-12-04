(ns adventofcode2018.day3)

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/3

(def input-data "resources/2018/day3-input.txt")
(def sample
  [{:id 1 :x 1 :y 3 :dx 4 :dy 4}
   {:id 2 :x 3 :y 1 :dx 4 :dy 4}
   {:id 3 :x 5 :y 5 :dx 2 :dy 2}])

(defn get-data
  "Parse the input file into a map we can use"
  []
  (map #(zipmap [:id :x :y :dx :dy] (map (fn [s] (Integer/parseInt s)) (rest %)))
       (re-seq #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)*" (slurp input-data))))

(defn coverage
  "Generate coordinates of all points in this square"
  [{:keys [x y dx dy]}]
  (for [px (range x (+ x dx)) py (range y (+ y dy))] [px py]))

(defn record-usage
  "Increment the usage count in the map for all points in this square"
  [m square]
  (reduce (fn [a p] (update a p (fnil inc 0))) m (coverage square)))

(defn create-usage-map
  "Create a map of coordinates with the sum of all overlaps >= 1"
  [data]
  (reduce record-usage {} data))

(defn overlap-count
  "Count how many coordinates in the usage map have more than 1 contributor"
  [data]
  (count (filter #(< 1 (second %)) (create-usage-map data))))

; (overlap-count (get-data))
; => 98005

; Part 2
(defn isolated
  "Find the ID of all squares that have 1 usage recorded in the usage map"
  [data]
  (let [usage (create-usage-map data)]
    (map :id
         (filter (fn [square]
                       (every? #(= 1 (get usage %)) (coverage square)))
                     data))))

; (isolated (get-data))
; => 331
