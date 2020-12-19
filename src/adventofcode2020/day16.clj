(ns adventofcode2020.day16)

(def input-data "resources/2020/day16-input.txt")

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (line-seq rdr))))

(defn build-rule [line]
  (let [[_ name s1 s2 s3 s4] (re-matches #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" line)
        [n11 n12 n21 n22] (map #(Integer/parseInt %) [s1 s2 s3 s4])]
    (fn [n] (or (<= n11 n n12) (<= n21 n n22)))))

(defn rule [problem line]
  (update problem :rules conj (build-rule line)))

(defn ticket [problem _] problem)

(defn build-ticket [line]
  (if (= line "nearby tickets:")
    nil
    (read-string (str "[" line "]"))))

(defn check-ticket [problem line]
  (let [ticket (build-ticket line)]
    (filter (fn [n] (not-any? #(% n) (:rules problem))) ticket)))

(defn nearby [problem line]
  (update problem :invalid concat (check-ticket problem line)))

(defn solve [lines]
  (loop [[section & next-sessions :as sections] [rule ticket nearby]
         [line & remaining] lines
         problem {}]
    (if (nil? line)
      (reduce + (:invalid problem))
      (recur (if (= line "") next-sessions sections)
             remaining
             (if (= line "") problem (section problem line))))))

(defn part1 [file]
  (process-file solve file))

; (part1 input-data)
; => 26988
