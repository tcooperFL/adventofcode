(ns adventofcode.day8)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/8
;; Part 1

(def day8-input-file "resources/day8-input.txt")
(def sample-data-file "resources/day8-sample-input.txt")

(def op-fns {"<" <, ">" >, "<=" <=, ">=" >=, "==" =, "!=" not=, "inc" +, "dec" -})

; Create an instruction from one-line description
(defn create-instruction [s]
  (let [[register op arg if-register if-op if-arg]
        (rest (re-find #"(\w+) (inc|dec) (-?\d+) if (\w+) (<|>|>=|<=|==|!=) (-?\d+)" s))]
    {:register    register
     :op          (op-fns op)
     :arg         (read-string arg)
     :if-register if-register
     :if-op       (op-fns if-op)
     :if-arg      (read-string if-arg)}))

; Read the input file into a sequence of program maps
(defn get-program [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (let [lines (line-seq rdr)]
      (doall (map create-instruction lines)))))

; Execute a program, returning the registers as a map of names and values, and max-value ever seen
(defn execute [program]
  (reduce (fn [{:keys [registers max-value]} {:keys [register op arg if-register if-op if-arg]}]
            (let [new-value (if (if-op (registers if-register 0) if-arg)
                              (op (registers register 0) arg)
                              (registers register 0))]
              {:registers (assoc registers register new-value)
               :max-value (max max-value new-value)}))
          {:registers {} :max-value 0}
          program))

; Execute the program and return the maximum final register value
(defn part1 [input-file]
  (apply max (vals (:registers (execute (get-program input-file))))))

; (part1 day8-input-file)
; => 4647

;; Part 2

; Execute the program and return the maximum value any register ever held
(defn part2 [input-file]
  (:max-value (execute (get-program input-file))))

; (part2 day8-input-file)
; => 5590
