(ns adventofcode2020.day14)

(def input-data "resources/2020/day14-input.txt")

;; File i/o
(defn process-file [f file]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (line-seq rdr))))

;; JIT compiler
(defmulti compile-mask (fn [part _] part))
(defmethod compile-mask :part1 [_ mask]
  (let [ops (->> (reverse mask)
                 (map-indexed
                   (fn [i bit] ({\1 #(bit-set % i) \0 #(bit-clear % i)} bit)))
                 (remove nil?))]
    (fn [machine]
      (assoc machine :mask (fn [v] (reduce #(%2 %1) v ops))))))

(defmulti compile-mem (fn [part _ _] part))
(defmethod compile-mem :part1 [_ address value]
  (let [a (Integer/parseInt address)
        v (Integer/parseInt value)]
    (fn [{:keys [mask] :as machine}]
      (assoc-in machine [:memory a] (mask v)))))

(defn compile-bad-instruction [num line]
  (printf "?? Bad instruction at line %s: \"%s\"\n" num line)
  identity)

(defn compile-instruction [part num line]
  (let [[_ op arg v] (re-matches #"(mask|mem)\[?(\d+)?\]? = (.+)" line)]
    (condp = op
      "mask" (compile-mask part v)
      "mem" (compile-mem part arg v)
      (compile-bad-instruction num line))))

;; Execution
(defn run [program]
  (reduce #(%2 %1) {:mask identity} program))

;; Run the program and sum the memory values
(defn solve [compiler lines]
  (->> (map-indexed (partial compile-instruction compiler) lines)
       run :memory vals (reduce +)))

(defn part1 [file]
  (process-file (partial solve :part1) file))

; (time (part1 input-data))
; "Elapsed time: 4.544217 msecs"
; => 9967721333886

(defn apply-floating [v [b & bs :as bits]]
  (if (nil? b)
    (list v)
    (let [applied (apply-floating v bs)]
      (concat (map #(bit-clear % b) applied)
              (map #(bit-set % b) applied)))))

(defmethod compile-mask :part2 [_ mask]
  (let [build (fn [f] (->> (reverse mask) (map-indexed f) (remove nil?)))
        ops (build (fn [i bit] ({\1 #(bit-set % i)} bit)))
        floating (build (fn [i bit] ({\X i} bit)))]
    (fn [machine]
      (assoc machine
        :mask
        (if floating
          (fn [address] (apply-floating (reduce #(%2 %1) address ops) floating))
          (fn [address] (list (reduce #(%2 %1) address ops))))))))

(defmethod compile-mem :part2 [_ address value]
  (let [a (Integer/parseInt address)
        v (Integer/parseInt value)]
    (fn [{:keys [mask memory] :as machine}]
      (assoc machine :memory (reduce #(assoc %1 %2 v) memory (mask a))))))

(defn part2 [file]
  (process-file (partial solve :part2) file))

; (time (part2 input-data))
; "Elapsed time: 63.008974 msecs"
; => 4355897790573
