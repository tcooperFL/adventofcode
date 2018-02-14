(ns adventofcode.day18)

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/18
;; Part 1

;; Problem setup data
(def input-file "resources/day18-input.txt")

(def sample "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2")

;; Value references
(defmulti value-of (fn [_ x] (number? x)))

(defmethod value-of true [computer x] x)

(defmethod value-of false [computer x]
  (get-in computer [:registers x] 0))

;; Instruction set
(defn snd-fn [computer arg _]
  (assoc computer :last-played (value-of computer arg)))

(defn set-fn [computer register arg]
  (assoc-in computer [:registers register] (value-of computer arg)))

(defn add-fn [computer register arg]
  (assoc-in computer [:registers register]
            (+ (value-of computer register)
               (value-of computer arg))))

(defn mul-fn [computer register arg]
  (assoc-in computer [:registers register]
            (* (value-of computer register)
               (value-of computer arg))))

(defn mod-fn [computer register arg]
  (assoc-in computer [:registers register]
            (mod (value-of computer register)
                 (value-of computer arg))))

(defn rcv-fn [computer arg _]
  (if-not (zero? (value-of computer arg))
    (do
      (println "Recovered sound:" (:last-played computer))
      (assoc computer :halted true))
    computer))

(defn jgz-fn [computer arg1 arg2]
  (if-not (zero? (value-of computer arg1))
    (assoc computer :pc (+ (value-of computer arg2) (dec (:pc computer))))
    computer))

(def part1-computer
  {:played nil
   :registers {}
   :pc 0
   :instruction-set {"snd" snd-fn, "set" set-fn, "add" add-fn, "mul" mul-fn, "mod" mod-fn, "rcv" rcv-fn, "jgz" jgz-fn}})

; Program
(defn compile-instruction [computer s]
  (let [[instruction arg1 arg2]
        (rest (re-find #"(\w+) (\w)(?: (-?.+))?" s))]
    {:inst-fn (get-in computer [:instruction-set instruction])
     :arg1    (read-string arg1)
     :arg2    (and arg2 (read-string arg2))}))

(defn compile-program [computer code]
  (zipmap (range) (map (partial compile-instruction computer) (clojure.string/split-lines code))))

(defn run-code [computer program]
  (let [{:keys [inst-fn arg1 arg2]} (get program (:pc computer))]
    (if-not (or (nil? inst-fn) (:halted computer))
      (recur (inst-fn (assoc computer :pc (inc (:pc computer))) arg1 arg2)))))

(defn part1
  ([infile] (part1 part1-computer infile))
  ([computer infile]
   (->> infile slurp (compile-program computer) (run-code computer))))

; (part1 input-file))
; Recovered sound: 8600

(defn send-fn [computer arg _]
  ; See concurrency book for best hand-off mechanism. Probably core.async channel.
  computer)

(defn receive-fn [computer arg _]
  ; See concurrency book for best hand-off mechanism. Probably core.async channel.
  ; TODO How do you detect deadlock here?
  computer)

(defn part2 [infile]
  (let [computer2 (assoc part1-computer
                    :instruction-set (merge (:instruction-set part1-computer) {"snd" send-fn, "rcv" receive-fn}))]
    (part1 computer2 infile)))


;; Part 2
;; Position 2 changes every time the spin wraps around, when the mod = 0


