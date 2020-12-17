(ns adventofcode2020.day14)

(def input-data "resources/2020/day14-input.txt")

;; File i/o
(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (line-seq rdr))))

;; JIT compiler
(defn compile-mask [mask]
  (let [ops (->> (reverse mask)
                 (map-indexed
                   (fn [i bit] ({\1 #(bit-set % i) \0 #(bit-clear % i)} bit)))
                 (remove nil?))]
    (fn [machine]
      (assoc machine
        :mask (fn [v] (reduce #(%2 %1) v ops))))))

(defn compile-mem [address value]
  (fn [{:keys [mask] :as machine}]
    (assoc-in machine
              [:memory (Integer/parseInt address)]
              (mask (Integer/parseInt value)))))

(defn compile-bad-instruction [num line]
  (printf "?? Bad instruction at line %s: \"%s\"\n" num line)
  identity)

(defn compile-instruction [num line]
  (let [[_ op arg v] (re-matches #"(mask|mem)\[?(\d+)?\]? = (.+)" line)]
    (condp = op
      "mask" (compile-mask v)
      "mem" (compile-mem arg v)
      (compile-bad-instruction num line))))

;; Execution
(defn run [program]
  (reduce #(%2 %1) {:mask identity} program))

;; Run the program and sum the memory values
(defn solve [lines]
  (->> (map-indexed compile-instruction lines)
       run :memory vals (reduce +)))

(defn part1 [file]
  (process-file solve file))

; (time (part1 input-data))
; "Elapsed time: 4.544217 msecs"
; => 9967721333886

