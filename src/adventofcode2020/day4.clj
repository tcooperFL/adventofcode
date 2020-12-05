(ns adventofcode2020.day4
  (:require [clojure.java.io :as io]))

(def input-data "resources/2020/day4-input.txt")

(defn digits?
  "Is this a string of digits, optionally n in length?"
  [s & [n]]
  (re-matches (if n (re-pattern (format "\\d{%d}" n)) #"\d+") s))

(def field-rules
  {"byr" #(and (digits? % 4) (<= 1920 (Integer/parseInt %) 2002))
   "iyr" #(and (digits? % 4) (<= 2010 (Integer/parseInt %) 2020))
   "eyr" #(and (digits? % 4) (<= 2020 (Integer/parseInt %) 2030))
   "hgt" #(let [[_ s units] (re-matches #"(\d+)(\w*)" %)
                v (Integer/parseInt s)]
            (case units
              "cm" (<= 150 v 193)
              "in" (<= 59 v 76)
              false))
   "hcl" #(not (nil? (re-matches #"#[0-9a-f]{6}" %)))
   "ecl" #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)
   "pid" #(not (nil? (re-matches #"[0-9]{9}" %)))})

(def required-keys (keys field-rules))

(defn build-passport
  [fields]
  (reduce
    (fn [m field]
      (let [[_ k v] (re-matches #"([^:]+):([^\s]+)" field)]
        (assoc m k v)))
    {}
    (reduce concat (map #(clojure.string/split % #"\s") fields))))

(defn passports [lines]
  (map build-passport (remove #{(list "")} (partition-by #{""} lines))))

(defn get-lines [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (line-seq rdr))))

(defn present? [p]
  (every? #(contains? p %) required-keys))

(defn part1
  "Read lines of the file return how many are valid passports"
  ([file] (part1 file present?))
  ([file valid-fn]
   (count (filter valid-fn (passports (get-lines file))))))

;;-> 262

(defn valid? [p]
  (and (present? p)
       (not-any? false? (map #((field-rules %) (get p %)) required-keys))))

(defn part2
  [file]
  (part1 file valid?))

;;-> 133
