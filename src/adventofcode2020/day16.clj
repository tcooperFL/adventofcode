(ns adventofcode2020.day16)

(def input-data "resources/2020/day16-input.txt")

(defn process-file [file f]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (line-seq rdr))))

(defn build-rule [line]
  (let [[_ title s1 s2 s3 s4] (re-matches #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" line)
        [n11 n12 n21 n22] (map #(Integer/parseInt %) [s1 s2 s3 s4])]
    {title (fn [n] (or (<= n11 n n12) (<= n21 n n22)))}))

(defn rule [problem line]
  (update problem :rules conj (build-rule line)))

(defn build-ticket [line]
  (if (= line "nearby tickets:")
    nil
    (read-string (str "[" line "]"))))

(defn invalid-fields [problem ticket]
  (filter (fn [n] (not-any? #(% n) (vals (:rules problem)))) ticket))

(defmulti nearby (fn [p _] (:part p)))
(defmethod nearby :part1 [problem line]
  (update problem :invalid concat (invalid-fields problem (build-ticket line))))

(defmulti ticket (fn [p _] (:part p)))
(defmethod ticket :part1 [problem _] problem)

(defn build-problem
  [problem lines]
  (loop [[step & next-steps :as all-steps] [rule ticket nearby]
         [line & remaining] lines
         problem problem]
    (if (nil? line)
      problem
      (recur (if (= line "") next-steps all-steps)
             remaining
             (if (= line "") problem (step problem line))))))

(defn part1 [file]
  (->> {:part :part1 :rules {}}
       (partial build-problem)
       (process-file file)
       :invalid
       (reduce +)))

; (part1 input-data)
; => 26988

(defn prune-fields [m numbers]
  (reduce (fn [fields [i n]]
            (update fields i #(filter (fn [rname] ((get-in m [:rules rname]) n)) %)))
          (:fields m)
          (map-indexed vector numbers)))

(defmethod nearby :part2 [problem line]
  (if-let [ticket (build-ticket line)]
    (if (empty? (invalid-fields problem ticket))
      (assoc problem :fields (prune-fields problem ticket))
      problem)
    problem))

(defmethod ticket :part2 [problem line]
  (if (= line "your ticket:")
    problem
    (let [my-ticket (build-ticket line)]
      (as-> problem p
            (assoc p :fields (zipmap (range (count my-ticket))
                                     (repeat (keys (:rules problem)))))
            (assoc p :my-ticket my-ticket)
            (nearby p line)))))

(defn remove-match [fields rule]
  (map (fn [[k lst]] [k (remove #{rule} lst)]) fields))

(defn find-single-assignment [fields]
  (seq (some #(if (= 1 (count (second %))) %) fields)))

(defn resolve-fields [fields]
  (if-let [[i [rule]] (find-single-assignment fields)]
    (cons [i rule] (resolve-fields (remove-match fields rule)))))

(defn compute-solution [mappings my-ticket]
  (->> mappings
       (filter #(clojure.string/starts-with? (second %) "departure"))
       (map #(nth my-ticket (first %)))
       (apply *)))

(defn part2 [file]
  (let [analysis (process-file file (partial build-problem {:part :part2 :rules {}}))
        field-mappings (resolve-fields (:fields analysis))]
    #_(println "Mappings:" field-mappings)
    (compute-solution field-mappings (:my-ticket analysis))))

; (part2 input-data)
; => 426362917709
