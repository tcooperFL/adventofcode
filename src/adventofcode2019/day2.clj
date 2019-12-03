(ns adventofcode2019.day2
  (:require [clojure.java.io :as io]))

(def input-data "resources/2019/day2-input.txt")

(def sample-data [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50])

(defn get-data
  "Parse the input file into an array of numbers"
  []
  (read-string (str "[" (slurp input-data) "]")))

(defn create-program [src]
  {:instructions (partition 4 4 (range))
   :positions    (zipmap (range) src)
   :running      true})

(defn step [{p-map :positions, i-map :instructions :as program}]
  (let [[code in1 in2 out] (map p-map (first i-map))]
    (assoc
      (condp = code
        99 (assoc program :running false)
        1 (assoc-in program [:positions out] (+ (p-map in1) (p-map in2)))
        2 (assoc-in program [:positions out] (* (p-map in1) (p-map in2)))
        (throw (IllegalStateException. (format "unrecognized code: %d" code))))
      :instructions (rest i-map))))

(defn display [{:keys [positions]}]
  (partition 4 4 (map positions (sort (keys positions)))))

(defn run [program]
  (first (drop-while :running (iterate step program))))

(defn run-with [{:keys [positions] :as p} noun verb]
  (get (:positions
         (run (assoc p :positions (merge positions {1 noun 2 verb}))))
       0))

(defn part1 []
  (run-with (create-program (get-data)) 12 2))

;; (part1) ==> 3224742

(defn find-pair [p target]
  (first (drop-while (fn [[n v]] (not= target (run-with p n v)))
                     (for [x (range 99) y (range 99)] [x y]))))

(defn part2 [target]
  (let [[n v] (find-pair (create-program (get-data)) target)]
    (+ (* n 100) v)))

;; (time (part2 19690720)) ==> 7960
;; "Elapsed time: 453.327262 msecs"
