(ns adventofcode2020.day18
  (:require [clojure.string :as s]))

(def input-data "resources/2020/day18-input.txt")

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (line-seq rdr))))

(def operation {"+" + "*" *})

(defn tokenize [s]
  (map #(or (get {"+" "+" "*" "*" "(" :begin ")" :end} %)
            (Integer/parseInt %))
       (s/split (s/replace s #"\s+" "") #"(?=\d+|\s+|[+\\*\\(\\)])")))

(defn push-value [v [top & tail] new-top]
  (if (string? (first top))
    (let [result ((operation (first top)) (second top) v)]
      (cons (cons result new-top) tail))
    (cons (cons v top) tail)))

(defn evaluate [s]
  (ffirst (reduce (fn [[top & tail :as stack] term]
                    (cond (= :begin term) (cons nil stack)
                          (= :end term) (push-value (first top) tail top)
                          (string? term) (cons (cons term top) tail)
                          (number? term) (push-value term stack (drop 2 top))))
                  '(())
                  (tokenize s))))

(defn solve [lines]
  (reduce + (map evaluate lines)))

(defn part1 [file]
  (process-file solve file))

; (part1 input-data)
; => 45840336521334