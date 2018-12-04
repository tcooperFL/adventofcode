(ns adventofcode2018.day2
  (:import [java.io BufferedReader StringReader]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/2

(def input-data "resources/2018/day2-input.txt")
(def sample ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(defn get-data
  "Parse the data into a sequence of lines"
  []
  (line-seq (BufferedReader. (StringReader. (slurp input-data)))))

; Frequency after processing all deltas
(defn checksum
  "Compute the checksum as the product of those with 2 duplicates and those with 3"
  [data]
  (let [freqs (map (comp set vals frequencies) data)
        f (fn [n] (count (filter #(% n) freqs)))]
    (* (f 2) (f 3))))

; (checksum) => 5368

; Part 2
(defn common
  "Find the chars in common with any 2 strings that differ by 1 character"
  [data]
  (let [[[s1 s2]]
        (for [[x & tail] (take (count data) (iterate rest (seq data))) y tail
              :when (= 1 (count (filter false? (map = (seq x) (seq y)))))]
          [x y])]
    (apply str
           (reduce (fn [a [c1 c2]] (if (= c1 c2) (conj a c1) a))
                   []
                   (map vector (seq s1) (seq s2))))))

; (common (get-data))
; => "cvgywxqubnuaefmsljdrpfzyi"