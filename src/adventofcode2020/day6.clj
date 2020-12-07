(ns adventofcode2020.day6)

(def input-data "resources/2020/day6-input.txt")

(defn process-file [t file]
  (with-open [rdr (clojure.java.io/reader file)]
    (into [] t (line-seq rdr))))

(defn yes-questions [line]
  (into #{} (clojure.string/replace line #"[^a-z]" "")))

(defn solve [file reducer]
  (->> (process-file (map yes-questions) file)
       (partition-by #{#{}})
       (map #(reduce reducer %))
       (remove empty?)
       (map count)
       (reduce +)))

(defn part1 [file]
  (solve file clojure.set/union))

;"Elapsed time: 12.618544 msecs"
;=> 6735

(defn part2 [file]
  (solve file clojure.set/intersection))

;"Elapsed time: 8.640875 msecs"
;=> 3221
