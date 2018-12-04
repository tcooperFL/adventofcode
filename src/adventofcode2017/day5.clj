(ns adventofcode2017.day5)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/5
;; Part 1

(def day5-input-file "resources/2017/day5-input.dat")
(def sample-data [0 3 0 1 -3])

; Read the input file into a sequence of ints
(defn get-data [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (let [lines (line-seq rdr)]
      (doall
        (map #(Integer/parseInt %) lines)))))

; We'll use a raw int array internally here for fast update and local mutation
(defn escape-step [data offset-fn]
  (let [arr (int-array data)]
    (loop [pos 0 step 0]
      (let [offset (get arr pos :escaped)]
        (if (= offset :escaped)
          step
          (do (aset arr pos (offset-fn offset))
              ;   (println step ":" (seq arr))
              (recur (+ pos offset) (inc step))))))))

; (escape-step (get-data day5-input-file) inc)
; => 358131


;; Part 2

(defn part2-offset [offset]
  (if (>= offset 3) (dec offset) (inc offset)))

; (escape-step (get-data day5-input-file) part2-offset)
; => 25558839
