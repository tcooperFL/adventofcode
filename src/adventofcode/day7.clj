(ns adventofcode.day7)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/7
;; Part 1

(def day7-input-file "resources/day7-input.txt")
(def sample-data-file "resources/day7-sample-input.txt")

; Create a program from a one-line iput description
(defn create-program [s]
  (let [[name weight children]
        (rest (re-matches #"(\w+)\s\((\d+)\)(?: -> (.*))?" s))]
    {name {:name     name
           :weight   (Integer/parseInt weight)
           :children (if children (clojure.string/split children #", "))}}))

; Read the input file into a sequence of program maps
(defn get-programs [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (let [lines (line-seq rdr)]
      (reduce merge
              (map create-program lines)))))

; The root is the one program that is not in the children of any other.
(defn root [m]
  (first (clojure.set/difference (set (keys m))
                                 (set (mapcat :children (vals m))))))

; (root (get-programs day7-input-file))
; => "cqmvs"


;; Part 2

; Identify the incorrect child and compute change to make to its weight.
; Then return what should be the correct sum of the child weights.
(defn rebalance [freq children]
  (let [min-v (apply min-key val freq)
        incorrect-weight (first min-v)
        [correct-weight _] (first (remove (partial = min-v) freq))
        correction (- correct-weight incorrect-weight)
        wrong-child (first (filter #(if (= incorrect-weight (:total-weight %)) %) children))]
    (println (format "Change %s weight to be %d\n"
                     (:name wrong-child)
                     (+ (:weight wrong-child) correction)))
    (* correct-weight (count children))))

; Sum up the total weights and look for unbalanced children
(defn weighted [programs]
  (letfn [(aux [p]
            (let [children (->> (:children p) (map programs) (map aux))
                  balance (frequencies (map :total-weight children))]
              (if (> (count balance) 1)
                (do
                  (println (apply str
                                  (cons (format "%s is unbalanced\n\t" (:name p))
                                        (map str (interpose "\n\t" children)))))
                  (assoc p :total-weight (+ (:weight p) (rebalance balance children))))
                (assoc p :total-weight (reduce + (:weight p) (map :total-weight children))))))]
    (:total-weight (aux (get programs (root programs))))))

; (weighted (get-programs day7-input-file))
; bntzksk is unbalanced
; {:name "vmttcwe", :weight 2318, :children ["zumdwwu" "lvazjz" "qhiav"], :total-weight 2615}
; {:name "ukwlfcf", :weight 1818, :children ["twblj" "vpbec" "wifaf"], :total-weight 2607}
; {:name "zzpevgd", :weight 17, :children ["smqtcaj" "emxwr" "uezxhid" "blvyrzh" "lvgrmfu" "vkuucpv" "eivaf"], :total-weight 2607}
; Change vmttcwe weight to be 2310
;
; => 315784
