(ns adventofcode2020.day18
  (:require [clojure.string :as s]
            [clojure.walk :refer :all]))

(def input-data "resources/2020/day18-input.txt")

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (line-seq rdr))))

(def ^:dynamic prec<
  "Default precedence is left associativity"
  (fn [_ _] false))

(defn tokenize [s]
  (map #(or (get {"+" "+" "*" "*" "(" :begin ")" :end} %)
            (Integer/parseInt %))
       (s/split (s/replace s #"\s+" "") #"(?=\d+|\s+|[+\\*\\(\\)])")))

(defn push-value [v [top & tail]]
  (if (< (count top) 3)
    (cons (conj top v) tail)
    (let [[op arg1 arg2] top]
      (cons [op arg1 (first (push-value v (list arg2)))] tail))))

(defn push-function [term [top & tail]]
  (if (number? top)
    (cons [term top] tail)
    (let [[op arg1 arg2] top]
      (cond (number? op) (cons [term op] tail)
            (prec< op term) (cons [op arg1 (first (push-function term [arg2]))] tail)
            :else (cons [term top] tail)))))

(defn parse
  "Parse the string into an AST"
  [s]
  (first (reduce (fn [[cur & pushed :as stack] term]
                   (cond (= :begin term) (cons nil stack)
                         (= :end term) (push-value (list cur) pushed)
                         (string? term) (push-function term stack)
                         (number? term) (push-value term stack)))
                 '(nil)
                 (tokenize s))))

(defn evaluate
  "Walk the AST to evaluate it"
  [ast]
  (let [operation {"+" + "*" *}]
    (postwalk #(cond (not (coll? %)) %
                    (number? (first %)) (first %)
                    :else (apply (operation (first %)) (rest %)))
              ast)))

(defn solve [lines]
  (->> lines (map parse) (map evaluate) (reduce +)))

(defn part1 [file]
  (process-file solve file))

; (part1 input-data)
; => 45840336521334

(defn part2
  "Change operator precedence and run part1 again."
  [file]
  (binding [prec< (fn [f1 f2] (and (= f1 "*") (= f2 "+")))]
    (part1 file)))

; (time (part2 input-data))
; "Elapsed time: 25.850787 msecs"
; => 328920644404583
