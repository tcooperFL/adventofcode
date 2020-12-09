(ns adventofcode2020.day8)

(def input-data "resources/2020/day8-input.txt")

;; Parsing

(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (mapv f (line-seq rdr)))))

(defn parse-line [line]
  (let [[op arg] (rest (re-matches #"(\w+) ([+-]\d+)" line))]
    {:operation (keyword op) :argument (Integer/parseInt arg)}))

(defn create-machine [instructions]
  {:accumulator  0
   :instructions instructions
   :pc           0
   :executed     #{}})

;; Machine execution

(defmulti execute :operation)
(defmethod execute :nop [_])
(defmethod execute :acc [{arg :argument}] [arg])
(defmethod execute :jmp [{arg :argument}] [nil arg])

(defn step
  "Execute one instruction and return the state of the machine"
  [{:keys [pc instructions accumulator executed] :as machine}]
  (let [[accum-increment pc-increment] (execute (nth instructions pc))]
    (assoc machine :pc (+ pc (or pc-increment 1))
                   :accumulator (+ accumulator (or accum-increment 0))
                   :executed (conj executed pc))))

(defn run-until
  "Run until you hit this termination state then return the accumulator"
  [terminated? machine]
  (first (drop-while (complement terminated?) (iterate step machine))))

(defn looping? [m] (contains? (:executed m) (:pc m)))

;; Main

(defn part1
  "Execute until you have executed this pc before, then return the accumulator"
  [file]
  (:accumulator (run-until looping? (create-machine (process-file parse-line file)))))

; (time (part1 input-data))
; "Elapsed time: 1.458506 msecs"
; => 1832

; Brute force, since this is fast enough: create a lazy sequence of machines, each varying
; from the original by a single jmp/nop flip

(defn machine-variations
  "Lazy sequence of all variations of the machine with one jmp/nop flipped."
  [m]
  (cons m
        (for [i (range (count (:instructions m))) :when (#{:nop :jmp} (:operation (nth (:instructions m) i)))]
          (update-in m [:instructions i :operation] {:nop :jmp :jmp :nop}))))

(defn completed? [m] (= (:pc m) (count (:instructions m))))

(defn part2
  "Search for the desired end state by swapping flipping nop and jmp statements"
  [file]
  (let [machine (create-machine (process-file parse-line file))]
    (:accumulator
      (some #(if (completed? %) %)                          ; Stop as soon as you find one completed
            (map (fn [m] (run-until #(or (looping? %) (completed? %)) m))
                 (machine-variations machine))))))

; (time (part2 input-data))
; "Elapsed time: 32.800338 msecs"
; => 662
