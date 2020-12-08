(ns adventofcode2020.day7)

(def input-data "resources/2020/day7-input.txt")

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (into {} (map f (line-seq rdr)))))

(defn parse-bag
  "Given a line describing a pair: bag name and contents
  [ <bagname> ( {:quantity <q> :name <name>}, ... ) ]"
  [s]
  (let [[_ bagname contents] (re-matches #"(\w+ \w+) bags contain (.*)" s)
        inner-bags (map (fn [[_ q n]]
                          (hash-map :quantity (Integer/parseInt q) :name n))
                        (re-seq #"(\d+) (\w+ \w+)" contents))]
    [bagname inner-bags]))

(defn found-in?
  "Lazily look for this bag name inside any bags and don't revisit bags"
  ([target bags bagname]
   (found-in? target bags (transient #{}) bagname))         ; Transient to share across call tree
  ([target bags seen bagname]
   (if-not (contains? seen bagname)                         ; Don't dive in again
     (let [contents (get bags bagname)]
       (conj! seen bagname)                                 ; Rememnber you've seen this
       (or (some #(= target (:name %)) contents)
           (first (drop-while (complement true?)
                              (map (partial found-in? target bags seen)
                                   (map :name contents)))))))))

(defn part1
  "Parse the file and count those bags where the target can be found somewhere."
  [file]
  (let [bags (process-file parse-bag file)
        target "shiny gold"]
    (count
      (filter (partial found-in? target bags)
              (remove #{target} (keys bags))))))

; (time (part1 input-data))
; "Elapsed time: 29.111116 msecs"
; => 378

(defn count-bags
  "Recursively count this bag and its contents."
  [bagname bags]
  (inc (reduce + (map #(* (:quantity %) (count-bags (:name %) bags))
                      (get bags bagname)))))

(defn part2
  "Parse the file and recursively count all the bags in the shiny gold bag."
  [file]
  (dec (count-bags "shiny gold" (process-file parse-bag file))))

; (time (part2 input-data))
; "Elapsed time: 2.173557 msecs"
; => 27526
