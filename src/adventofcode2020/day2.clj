(ns adventofcode2020.day2
  (:require [clojure.java.io :as io]))

(def input-data "resources/2020/day2-input.txt")

(defn get-data
  "Read instructions from the file and return as a seq of strings"
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (line-seq rdr))))

(defn build-problem
  "Parse a line and put into a map that phrases the problem"
  [s]
  (let [[_ smin smax schar spassword] (re-matches #"(\d+)-(\d+) (\w): ([a-z]+)" s)]
    {:p1 (Integer/parseInt smin)
     :p2 (Integer/parseInt smax)
     :char (first schar)
     :password spassword}))

(defn check-password1
  "Evaluate the problem and return true if the password matches re: part 1"
  [{:keys [p1 p2 char password]}]
  (let [occurrences (count (filter #{char} password))]
    (and (>= occurrences p1)
         (<= occurrences p2))))

(defn passwords-succeeded [policy]
  (->> (get-data input-data)
       (map build-problem)
       (map policy)
       (remove false?)
       count))

(defn part1 []
  (passwords-succeeded check-password1))

;;-> 603

(defn check-password2
  "Evaluate the password based on policy in part 2"
  [{:keys [p1 p2 char password]}]
  (let [c1 (nth password (dec p1))
        c2 (nth password (dec p2))]
    (= 1 (count (filter #{char} [c1 c2])))))

(defn part2 []
  (passwords-succeeded check-password2))

;;-> 404
