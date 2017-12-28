(ns adventofcode.day6)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/6
;; Part 1

(def day6-input [5	1	10	0	1	7	13	14	3	12	8	10	7	12	0	6])

(def sample-input [0 2 7 0])

(defn bank-at [b n] (mod n (count b)))

(defn redistribute [banks [max-bank value]]
  (reduce (fn [b n]
            (assoc b (bank-at b n) (inc (get b (bank-at b n)))))
          (assoc banks max-bank 0)
          (range (inc max-bank) (+ max-bank value 1))))

(defn first-max-value-pair
  "Return the first [k v] for which v is the greatest"
  [m]
  (reduce (fn [[mk mv] [k v]]
            (if (> v mv) [k v] [mk mv]))
          (map #(vector %1 (get m %1)) (range 0 (count m)))))

(defn run-redistributions
  ([input] (run-redistributions conj (zipmap (range) input)))
  ([note-fn init-banks]
   (loop [seen #{} banks init-banks cycles 0]
     (if (seen banks)
       {:cycles cycles :banks banks}
       (recur (note-fn seen banks)
              (redistribute banks (first-max-value-pair banks))
              (inc cycles))))))

(defn cycles-til-looping [input]
  (:cycles (run-redistributions input)))

; (cycles-til-looping day6-input)
; => 5042

;; Part2

(defn cycle-loop-size [input]
  (let [{:keys [banks]} (run-redistributions input)]
    (:cycles (run-redistributions (constantly #{banks}) banks))))

; (cycle-loop-size day6-input)
; => 1086
