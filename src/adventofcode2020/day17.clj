(ns adventofcode2020.day17
  (:require [clojure.set :refer [union difference]]))

(def input-data "resources/2020/day17-input.txt")

(def neighbors3d
  (let [functions [dec identity inc]]
    (for [fx functions fy functions fz functions
          :when (not= [fx fy fz] [identity identity identity])]
      [fx fy fz])))

(def neighbors4d
  (let [functions [dec identity inc]]
    (for [fx functions fy functions fz functions fw functions
          :when (not= [fx fy fz fw] [identity identity identity identity])]
      [fx fy fz fw])))

;; Parsing

(defn parse-line [[y line]]
  (map #(vector (first %) y)
       (filter #(= (second %) \#) (map-indexed vector (seq line)))))

(defn process-file [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (mapcat parse-line (map-indexed vector (line-seq rdr))))))

; Yeah, this feels like cheating, but a fully generic version would be a pain.
(defmulti neighbors-of count)
(defmethod neighbors-of 3 [[x y z]]
  (map (fn [[fx fy fz]] [(fx x) (fy y) (fz z)]) neighbors3d))

(defmethod neighbors-of 4 [[x y z w]]
  (map (fn [[fx fy fz fw]] [(fx x) (fy y) (fz z) (fw w)]) neighbors4d))

(defn inactive-neighbors [actives]
  (reduce (fn [result active]
            (union result (remove actives (neighbors-of active))))
          #{}
          actives))

(defn create-simulation [coords dims]
  (let [actives (set (map (fn [c] (first (drop (- dims 2)
                                               (iterate #(conj % 0) c))))
                          coords))]
    {:cycles   0
     :active   actives
     :inactive (set (inactive-neighbors actives))}))

(defn neighbor-count [neighbors coord]
  (count (filter neighbors (neighbors-of coord))))

(defn run-cycle [{:keys [cycles active inactive] :as sim}]
  (let [deactivate (into #{} (filter #(not (#{2 3} (neighbor-count active %))) active))
        status (group-by #(neighbor-count active %) inactive)
        adding-active (get status 3 #{})
        inactive-neighbors (mapcat #(remove active (neighbors-of %)) adding-active)]
    (assoc sim :cycles (inc cycles)
               :active (union (difference active deactivate) (set adding-active))
               :inactive (union deactivate
                                (difference inactive (set (get status 0 #{})))
                                (set inactive-neighbors)))))

(defn solve [file dims]
  (count (:active (first (drop 6 (iterate run-cycle
                                          (create-simulation (set (process-file file)) dims)))))))
(defn part1 [file] (solve file 3))

; (part1 input-data)
; => 240

(defn part2 [file] (solve file 4))

; (time (part2 input-data))
; "Elapsed time: 1653.946977 msecs"
; => 1180
