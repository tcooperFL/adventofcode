(ns adventofcode2017.day18
  (:require [clojure.core.async :refer [>! >!! <! <!! go chan buffer close! thread alts! alts!! timeout]]))

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/18
;; Part 1

;; Problem setup data
(def input-file "resources/2017/day18-input.txt")

(def sample "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2")

;; Values based on the reference type
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

(defn jgz-fn [computer x y]
  (if (> (value-of computer x) 0)
    (assoc computer :pc (+ (value-of computer y) (dec (:pc computer))))
    computer))

; Create a computer prototype
(def computer-prototype
  {:played          nil
   :registers       {}
   :pc              0
   :instruction-set {"snd" snd-fn, "set" set-fn, "add" add-fn, "mul" mul-fn,
                     "mod" mod-fn, "rcv" rcv-fn, "jgz" jgz-fn}})

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
  (loop [next-state (assoc computer :executed 0)]
    (let [{:keys [inst-fn arg1 arg2]} (get program (:pc next-state))]
      (if-not (or (nil? inst-fn) (:halted next-state))
        (recur (assoc (inst-fn (assoc next-state :pc (inc (:pc next-state))) arg1 arg2)
                 :executed (inc (:executed next-state))))
        next-state))))

(defn part1
  ([infile] (part1 computer-prototype (slurp infile)))
  ([computer program]
   (->> program (compile-program computer) (run-code computer))))

; (part1 input-file))
; Recovered sound: 8600

;; Part 2
;; snd rcv is async send/receive

; Two new instruction implementations
(defn send-fn [computer arg _]
  (>!! (:write-chan computer) (value-of computer arg))
  (assoc computer :sent-count (inc (get computer :sent-count 0))))

(defn receive-fn [computer register _]
  (if-let [x (first (alts!! [(:read-chan computer) (timeout 2000)]))]
    (assoc-in computer [:registers register] x)
    (assoc computer :halted true)))

; Construct a part2 computer to override those instructions and provide read and write channels
(defn part2-computer [id read-chan write-chan]
  (assoc computer-prototype
    :instruction-set (merge (:instruction-set computer-prototype) {"snd" send-fn, "rcv" receive-fn})
    :registers (merge (:registers computer-prototype) {'p id})
    :read-chan read-chan
    :write-chan write-chan
    :id id))

(defn part2 [infile]
  (let [chan0 (chan 100) chan1 (chan 100)
        program (slurp infile)
        computer0 (part2-computer 0 chan0 chan1)
        computer1 (part2-computer 1 chan1 chan0)]
    (thread (part1 computer0 program))
    (println "Computer 1 ran"
             (:sent-count (part1 computer1 program))
             "instructions")))

; (part2 (slurp input-file))
; Computer 1 ran 7239 instructions
