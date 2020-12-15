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

(defmulti execute (fn [op pos] [(first op) (:part pos)]))
(defmethod execute ["N" :part1] [[_ arg] p] (update p :sy + arg))
(defmethod execute ["S" :part1] [[_ arg] p] (update p :sy - arg))
(defmethod execute ["E" :part1] [[_ arg] p] (update p :sx + arg))
(defmethod execute ["W" :part1] [[_ arg] p] (update p :sx - arg))
(defmethod execute ["L" :part1] [[_ arg] p] (update p :facing turn-left arg))
(defmethod execute ["R" :part1] [[_ arg] p] (update p :facing turn-right arg))
(defmethod execute ["F" :part1] [[_ arg] p] (execute [(:facing p) arg] p))

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

(defn move [pos instruction] (execute instruction pos))

(defn create-position1 [] {:part :part1 :sx 0 :sy 0 :facing "E"})

(defn solve [start instructions]
  (let [result (reduce move start instructions)]
    (+ (Math/abs ^long (:sx result)) (Math/abs ^long (:sy result)))))

(defn part1 [file]
  (process-file (partial solve (create-position1)) file))

; (time (part1 input-data))
; "Elapsed time: 2.309051 msecs"
; => 2879

(defmethod execute ["N" :part2] [[_ arg] p] (update p :dy + arg))
(defmethod execute ["S" :part2] [[_ arg] p] (update p :dy - arg))
(defmethod execute ["E" :part2] [[_ arg] p] (update p :dx + arg))
(defmethod execute ["W" :part2] [[_ arg] p] (update p :dx - arg))

(defn rotate [[x y] radians]
  (let [cos-r (Math/cos radians)
        sin-r (Math/sin radians)]
    (mapv #(Math/round ^double %)
          [(- (* x cos-r) (* y sin-r) )
           (+ (* y cos-r) (* x sin-r))])))

(defn rotate-waypoint [{:keys [dx dy]} degrees]
  (rotate [dx dy] (Math/toRadians degrees)))

(defmethod execute ["L" :part2] [[_ degrees] p]
  (let [[x y] (rotate-waypoint p degrees)]
    (update (assoc p :dx x :dy y) :facing turn-left degrees)))

(defmethod execute ["R" :part2] [[_ degrees] p]
  (let [[x y] (rotate-waypoint p (- 360 degrees))]
    (update (assoc p :dx x :dy y) :facing turn-left degrees)))

(defmethod execute ["F" :part2] [[_ n] {:keys [dx dy sx sy] :as p}]
  (assoc p :sx (+ sx (* n dx)) :sy (+ sy (* n dy))))

(defn create-position2 [] {:part :part2 :dx 10 :dy 1 :sx 0 :sy 0 :facing "E"})

(defn part2 [file]
  (process-file (partial solve (create-position2)) file))

; (time (part2 input-data))
; "Elapsed time: 9.479058 msecs"
; => 178986