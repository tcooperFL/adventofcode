(ns adventofcode2020.day9)

(def input-data "resources/2020/day9-input.txt")

(def preamble-size 25)

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (map #(Integer/parseInt %) (line-seq rdr)))))

(defn find-sum [target xs y-set]
  (some #(contains? y-set (- target %)) xs))

(defn solve-part1 [input]
  (loop [numbers input
         preamble (apply sorted-set (take preamble-size input))]
    (let [target (nth numbers preamble-size)]
      (if-not (find-sum target (take-while #(<= % (quot target 2)) preamble) preamble)
        target
        (recur (drop 1 numbers)
               (conj (disj preamble (first numbers)) target))))))

(defn part1 [file]
  (process-file solve-part1 input-data))

; (time (part1 input-data))
; "Elapsed time: 2.159166 msecs"
; => 22406676

