(ns adventofcode2020.day15)

(def input-data "0,20,7,16,1,18,15")

(defn speak [last-time-spoken turn]
  (if (nil? last-time-spoken) 0 (- turn last-time-spoken)))

(defn solve [data limit]
  (loop [turn (count data)
         spoken (last data)
         history (zipmap (butlast data) (range 1 (count data)))]
    (if (>= turn limit)
      spoken
      (recur (inc turn)
             (speak (history spoken) turn)
             (assoc history spoken turn)))))

(defn part1 [input]
  (solve (read-string (str "[" input "]")) 2020))

; (part1 input-data)
; => 1025

(defn part2 [input]
  (solve (read-string (str "[" input "]")) 30000000))

; (time (part2 input-data))
; "Elapsed time: 16944.543685 msecs"
; => 129262
