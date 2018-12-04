(ns adventofcode.day4)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/4
;; Part 1

(def day4-input-file "resources/day4-input.txt")

(defn no-duplicates? [words]
  (= (count words) (count (set words))))

(defn count-valid-lines [input-file pred]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> rdr
         line-seq
         (map #(clojure.string/split % #"\s+"))
         (filter pred)
         count)))

; (count-valid-lines day4-input-file no-duplicates?)
;  --> 477


;; Part 2

(defn no-anagrams? [words]
  (= (count words)
     (count (reduce conj #{} (map (comp seq sort) words)))))

; (count-valid-lines day4-input-file no-anagrams?)
; --> 167