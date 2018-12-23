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

;; A graph is in this form: { "A" {:succ #{"B" "C"} :pred #{...} }, ... }
(defn create-graph
  "Create predecessors and successor for each step."
  [data]
  (reduce (fn [m [pred succ]]
            (assoc-in
              (assoc-in m [pred :succ] (conj (get-in m [pred :succ] #{}) succ))
              [succ :pred] (conj (get-in m [succ :pred] #{}) pred)))
          {} data))

(defn next-steps
  "Return the next steps in alphabetic order that have no predecessors."
  ([g] (next-steps (keys g) g #{}))
  ([steps dependencies ignore]
   (sort (filter #(empty? (reduce disj (get-in dependencies [% :pred]) ignore)) steps))))

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
    (if-let [next (first (next-steps steps))]
      (recur
        (drop-step steps next)
        (conj path next))
      (apply str path))))

(defn part1 [g]
  (->> g create-graph step-ordering))

; (part1 (get-data (slurp input-data)))
; => "BGKDMJCNEQRSTUZWHYLPAFIVXO"

; Part 2

(defn work-time
  "The time it takes to do this step"
  [fixed s]
  (+ 1 fixed (- (int (first s)) (int \A))))

(defn create-schedule [g w f]
  {:dependencies g
   :todo         (set (keys g))
   :ready        #{}
   :in-progress  '()
   :done         []
   :workers      w
   :fixed        f})

(defn tick
  "Do one unit of work and return the state after that is done."
  [g]
  (let [in-progress (map #(update-in % [1] dec) (:in-progress g))
        just-finished (set (map first (filter (comp zero? second) in-progress)))
        done (reduce conj (:done g) just-finished)
        ready (reduce conj (:ready g) (next-steps (:todo g) (:dependencies g) done))
        starting (take (- (:workers g) (- (count in-progress) (count just-finished)))
                       (sort ready))]
    (merge g
           {:done        done
            :in-progress (reduce conj
                                 (remove #(just-finished (first %)) in-progress)
                                 (map #(vector % (work-time (:fixed g) %)) starting))
            :ready       (reduce disj ready starting)
            :todo        (reduce disj (:todo g) ready)})))

(defn done?
  "We are done when all the steps are done."
  [sched]
  (= (count (:dependencies sched)) (count (:done sched))))

(defn do-work
  "Create a stream of states corresponding to the state after each second of work."
  [sched]
  (map-indexed vector (iterate tick (tick sched))))

(defn schedule-ordering
  "Work on the steps until finished, and return the final clock time and state"
  [g workers fixed]
  (first
    (drop-while #(not (done? (second %)))
                (do-work (create-schedule g workers fixed)))))

(defn part2 []
  ; Myself + 4 helper elves = 5 workers
  (-> (slurp input-data) get-data create-graph (schedule-ordering 5 60)))

; (part2)
; => [941 ...]
