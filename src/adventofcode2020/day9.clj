(ns adventofcode2020.day9)

(def input-data "resources/2020/day9-input.txt")

(def preamble-size 25)

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (map #(Integer/parseInt %) (line-seq rdr)))))

(defn find-sum [target xs y-set]
  (some #(contains? y-set (- target %)) xs))

(defn solve-part1
  "Given a lazy sequence of longs, return the first that isn't a sum of 2 of the 25 preceding numbers"
  [input]
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

(defn collect
  "Take numbers from the stream while the sum is <= the target"
  [sum stream target]
  (loop [total sum collected nil [n & remaining] stream]
    (if (or (nil? n) (> (+ total n) target))
      [total (reverse collected)]
      (recur (+ total n)
             (cons n collected)
             remaining))))

(defn sum-max-and-min
  "Return the sum of the largest and smallest numbers in this collection"
  [coll]
  (+ (apply max coll) (apply min coll)))

(defn solve-part2
  "Given a lazy sequence of longs, return the min and max values of the first consecutive string that adds to a target"
  [target input]
  (loop [sum 0 addends nil stream input]
    (let [[new-sum extension] (collect sum stream target)]
      (if (= new-sum target)
        (sum-max-and-min (concat addends extension))
        (recur (- new-sum (or (first addends) 0))
               (concat (rest addends) extension)
               (drop (count extension) stream))))))

(defn part2 [file]
  (process-file (partial solve-part2 22406676) input-data))

; (time (part2 input-data))
; "Elapsed time: 9.850463 msecs"
; => 2942387
