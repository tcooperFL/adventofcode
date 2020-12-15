(ns adventofcode2020.day12)

(def input-data "resources/2020/day12-input.txt")

;; Directions and relative turning

(def directions ["N" "E" "S" "W"])

(def turn-map
  "Caching directional map with { <degree> <direction> ...}"
  (memoize (fn [directions]
             (reduce (fn [m dir]
                       (assoc m dir
                                (zipmap [0 90 180 270]
                                        (take 4 (drop-while #(not= % dir)
                                                            (cycle directions))))))
                     {}
                     directions))))

(defn turn-left [from degrees]
  (get ((turn-map (reverse directions)) from) degrees))

(defn turn-right [from degrees]
  (get ((turn-map directions) from) degrees))

;; Executing instructions

(defmulti execute (fn [pos op] [(:part pos) (first op)]))
(defmethod execute [:part1 "N"] [p [_ arg]] (update p :sy + arg))
(defmethod execute [:part1 "S"] [p [_ arg]] (update p :sy - arg))
(defmethod execute [:part1 "E"] [p [_ arg]] (update p :sx + arg))
(defmethod execute [:part1 "W"] [p [_ arg]] (update p :sx - arg))
(defmethod execute [:part1 "L"] [p [_ arg]] (update p :facing turn-left arg))
(defmethod execute [:part1 "R"] [p [_ arg]] (update p :facing turn-right arg))
(defmethod execute [:part1 "F"] [p [_ arg]] (execute p [(:facing p) arg]))

(defn create-instruction
  "Given the instruction string, return a function taking a position and returning a new one"
  [s]
  (let [[_ op arg] (re-matches #"([A-Z])(\d+)" s)]
    [op (Integer/parseInt arg)]))

;; File i/o

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (map create-instruction (line-seq rdr)))))

;; Solution

(defn create-position1 [] {:part :part1 :sx 0 :sy 0 :facing "E"})

(defn solve [start instructions]
  (let [result (reduce execute start instructions)]
    (+ (Math/abs ^long (:sx result)) (Math/abs ^long (:sy result)))))

(defn part1 [file]
  (process-file (partial solve (create-position1)) file))

; (time (part1 input-data))
; "Elapsed time: 2.309051 msecs"
; => 2879

(defmethod execute [:part2 "N"] [p [_ arg]] (update p :dy + arg))
(defmethod execute [:part2 "S"] [p [_ arg]] (update p :dy - arg))
(defmethod execute [:part2 "E"] [p [_ arg]] (update p :dx + arg))
(defmethod execute [:part2 "W"] [p [_ arg]] (update p :dx - arg))

(defn rotate [[x y] radians]
  (let [cos-r (Math/cos radians)
        sin-r (Math/sin radians)]
    (mapv #(Math/round ^double %)
          [(- (* x cos-r) (* y sin-r))
           (+ (* y cos-r) (* x sin-r))])))

(defn rotate-waypoint [{:keys [dx dy]} degrees]
  (rotate [dx dy] (Math/toRadians degrees)))

(defmethod execute [:part2 "L"] [p [_ degrees]]
  (let [[x y] (rotate-waypoint p degrees)]
    (update (assoc p :dx x :dy y) :facing turn-left degrees)))

(defmethod execute [:part2 "R"] [p [_ degrees]]
  (let [[x y] (rotate-waypoint p (- 360 degrees))]
    (update (assoc p :dx x :dy y) :facing turn-left degrees)))

(defmethod execute [:part2 "F"] [{:keys [dx dy sx sy] :as p} [_ n]]
  (assoc p :sx (+ sx (* n dx)) :sy (+ sy (* n dy))))

(defn create-position2 [] {:part :part2 :dx 10 :dy 1 :sx 0 :sy 0 :facing "E"})

(defn part2 [file]
  (process-file (partial solve (create-position2)) file))

; (time (part2 input-data))
; "Elapsed time: 9.479058 msecs"
; => 178986