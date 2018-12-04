(ns adventofcode2018.day4)

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/4

(def input-data "resources/2018/day4-input.txt")

(defn parse-action
  "Parse the action and return a map of information derived from it"
  [s]
  (cond
    (.startsWith s "falls") {:action :sleep}
    (.startsWith s "wakes") {:action :wake}
    :default {:acton :start
              :guard (Integer/parseInt (second (re-matches #"Guard #(\d+).*" s)))}))

(defn sleep-records
  "Transform the data into sleep records with keys [:guard :sleep :duration]"
  [data]
  (loop [[r & remaining] data guard nil sleep-started nil result []]
    (if (nil? r)
      result
      (let [g (get r :guard guard)
            s (if (= (:action r) :sleep) (:minute r) sleep-started)]
        (recur remaining g s
               (if (= (:action r) :wake)
                 (conj result
                       {:guard g :sleep sleep-started :duration (- (:minute r) sleep-started)})
                 result))))))

(defn get-data
  "Parse the input records as a string into a map sorted by time"
  [s]
  (->>
    (re-seq #"\[(\d+)-(\d+)-(\d+) (\d\d):(\d\d)\] (.*)" s)
    (map (fn [[_ y m d h minute action]]
           (merge
             {:time   (Long/parseLong (str y m d h minute))
              :minute (Long/parseLong minute)}
             (parse-action action)))
         )
    (sort-by :time)))

(defn sleepy-guard
  "What guard has the most sleep time?"
  [records]
  (->>
    (group-by :guard records)
    (map (fn [[x y]] [x (reduce + (map :duration y))]))
    (sort-by second)
    last
    first))

(defn sleepy-minute
  "What minute does this guard sleep the most?"
  [records guard]
  (->>
    (get (group-by :guard records) guard)
    (reduce (fn [a {:keys [sleep duration]}]
              (reduce #(assoc %1 %2 (inc (get %1 %2 0)))
                      a
                      (range sleep (+ sleep duration))))
            {})
    (apply (partial max-key second))
    ((fn [[m t]] {:guard guard :minute m :times t}))))

(defn part1 []
  (let [records (-> input-data slurp get-data sleep-records)
        g (-> records sleepy-guard)]
    (* g (:minute (sleepy-minute records g)))))

; (part1)
; => 106710

; Part 2

(defn part2 []
  (let [records (-> input-data slurp get-data sleep-records)]
    (->>
      (keys (group-by :guard records))
      (map (partial sleepy-minute records))
      (sort-by :times)
      last
      ((fn [{:keys [guard minute]}] (* guard minute))))))

; (part2)
; => 10491
