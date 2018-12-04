(ns adventofcode2017.day2)

;; Advent of Code challenges
;; https://adventofcode.com/2017/day/2

(def day2-input-file "resources/2017/day2-input.dat")

(defn process-file [input-file tokenize-fn parse-fn row-calc-fn]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> rdr
         line-seq
         (map tokenize-fn)
         (map parse-fn)
         (map row-calc-fn)
         (reduce +))))

(defn checksum
  "Simple checksum with (max - min) for each row"
  [input-file]
  (process-file input-file
                #(re-seq #"\d+" %)
                #(map read-string %)
                #(- (apply max %) (apply min %))))

(defn checksum2
  "Variation of checksum using ratio of elements in the row that are evently dividable"
  [input-file]
  (process-file input-file
                #(re-seq #"\d+" %)
                #(map read-string %)
                #(first (for [dividend %
                              divisor %
                              :let [ratio (/ dividend divisor)]
                              :when (and (> ratio 1)
                                         (= (type ratio) java.lang.Long))]
                          ratio))))

; (checksum day2-input-file)
