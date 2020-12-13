(ns adventofcode2020.day10)

(def input-data "resources/2020/day10-input.txt")

(defn get-data [file]
  (read-string (format "[%s]" (slurp file))))

(defn connect [c]
  (cons 0 (sort (conj c (+ 3 (apply max c))))))

(defn diffs [chain]
  (map (fn [[a1 a2]] (- a2 a1)) (partition 2 1 chain)))

(defn solve1 [adapters]
  (let [analysis
        (->> adapters connect diffs frequencies)]
    (* (get analysis 1) (get analysis 3))))

(defn part1 [file]
  (solve1 (get-data file)))

; (time (part1 input-data))
; "Elapsed time: 0.901147 msecs"
; => 2812

(defn subchains
  "Group the adapters into chains of independent subgraphs"
  [adapters]
  (let [acc (atom []) result (atom [])]
    (doseq [[a1 a2] (partition 2 1 adapters)]
      (if (< (- (or a2 a1) a1) 3)
        (swap! acc conj a1)
        (do
          (swap! result conj (conj @acc a1))
          (reset! acc []))))
    (conj @result @acc)))

(defn count-paths [[head & tail]]
  "Recursively count paths through this chain skipping up to 3 away"
  (if (empty? tail)
    1
    (apply + (map count-paths
                  (take-while #(and (seq %) (<= (- (first %) head) 3))
                              (iterate rest (seq tail)))) )))

(defn solve2 [adapters]
  (->> adapters connect subchains (map count-paths) (reduce *)))

(defn part2 [file]
  (solve2 (get-data file)))

; (time (part2 input-data))
; "Elapsed time: 1.262358 msecs"
; => 386869246296064
