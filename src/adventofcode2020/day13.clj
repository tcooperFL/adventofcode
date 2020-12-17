(ns adventofcode2020.day13)

(def input-data "resources/2020/day13-input.txt")

;; File i/o

(defn get-data [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (line-seq rdr))))

(defn parse-data [[ts schedule]]
  [(Integer/parseInt ts)
   (->> (clojure.string/split schedule #",")
        (remove #{"x"})
        (map #(Integer/parseInt %)))])

(defn wait [ts departure]
  (- departure (rem ts departure)))

(defn solve [[ts deps]]
  (apply (partial min-key :wait)
         (map #(hash-map :dep % :wait (wait ts %)) deps)))

;; Solution part 1
(defn part1 [file]
  (let [{:keys [dep wait]} (solve (parse-data (get-data file)))]
    (* dep wait)))

; (time (part1 input-data))
; "Elapsed time: 0.495503 msecs"
; => 5257

(defn parse-data2 [[_ schedule]]
  (->> (clojure.string/split schedule #",")
       (map-indexed vector)
       (remove #(= (second %) "x"))
       (map (fn [[i departure]] [i (Integer/parseInt departure)]))))

(defn align
  "Search for the next bus to line up and return new time and step"
  [{:keys [t step]} [offset id]]
  (let [goal (mod offset id)]
    (some #(if (= goal (rem (- id (rem % id)) id))
             {:t % :step (* step id)})
          (iterate (partial + step) t))))

(defn solve2 [[[delay id] :as schedule]]
  (reduce align {:t (- id (mod delay id)) :step 1} schedule))

;; Solution part 2
(defn part2 [file]
  (->> file get-data parse-data2 (sort-by second) reverse solve2 :t))

; (time (part2 input-data))
; "Elapsed time: 0.606444 msecs"
; => 538703333547789

