(ns adventofcode2018.day7
  (:import [java.io BufferedReader StringReader]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/7

(def input-data "resources/2018/day7-input.txt")

(def sample "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.")

(defn get-data
  "Parse the data into pairs: [X Y] where X must come before Y"
  [s]
  (map #(rest (re-matches #"Step (\w) must be finished before step (\w).*" %))
       (line-seq (BufferedReader. (StringReader. s)))))

(defn create-graph
  "Create predecessors and successor for each step."
  [data]
  (reduce (fn [m [pred succ]]
            (assoc-in
              (assoc-in m [pred :succ] (conj (get-in m [pred :succ] #{}) succ))
              [succ :pred] (conj (get-in m [succ :pred] #{}) pred)))
          {} data))

(defn choose-next
  "Pick the best next step that has no predecessors."
  [g]
  (first (sort (filter #(empty? (get-in g [% :pred])) (keys g)))))

(defn drop-step
  "Remove this step from the graph and from all its successor's pred sets"
  [g step]
  (reduce #(update-in %1 [%2 :pred] disj step)
          (dissoc g step)
          (get-in g [step :succ])))

(defn step-ordering
  "Look through picking the next best step with no predecessors until we're done."
  [g]
  (loop [steps g path []]
    (if-let [next (choose-next steps)]
      (recur
        (drop-step steps next)
        (conj path next))
      (apply str path))))

(defn part1 [g]
  (->> g create-graph step-ordering))

; (part1 (get-data (slurp input-data)))
; =>

; Part 2

; TBS

; (part2 (get-data (slurp input-data))
; =>