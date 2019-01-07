(ns adventofcode2018.day15
  (:require [clojure.java.io :as io])
  (:use util.trace
        clojure.repl
        clojure.pprint))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/15

(def input-data "resources/2018/day15-input.txt")
(def sample-data "resources/2018/day15-sample.txt")

(defmulti create-unit (fn [ch] ch))

(defmethod create-unit \E [_]
  {:type         :elf
   :id           (gensym "E-")
   :display      \E
   :hit-points   200
   :attack-power 3})

(defmethod create-unit \G [_]
  {:type         :goblin
   :id           (gensym "G-")
   :display      \G
   :hit-points   200
   :attack-power 3})

(defmethod create-unit \# [_]
  {:type :wall})

(def enemy {:elf :goblin, :goblin :elf})

;; Begin debugging support
(defn print-map
  "Print the map in the same form as the adventofcode examples"
  [combat]
  (let [[x1 y1] (first (:walls combat))
        [x2 y2] (last (:walls combat))]
    (doseq [y (range y1 (inc y2))]
      (doseq [x (range x1 (inc x2))]
        (print
          (if (contains? (:walls combat) [x y])
            \#
            (if-let [unit (get (:units combat) [x y])]
              (:display unit)
              '.))))
      (println))))

(defn log-round [round]
  (if (:updated round)
    (do (print "After" (:rounds round) "rounds:\n") (print-map round))
    (println "Combat ends after" (dec (:rounds round)) "rounds.\nTotal hit points"
             (reduce + (map :hit-points (vals (:units round)))))))
;; End debugging support

(defn loc-comparator
  "Sort comparator function for locations"
  [[x1 y1] [x2 y2]]
  (if (= y1 y2) (compare x1 x2) (compare y1 y2)))

;; Combat state functions

(defn create-combat-state
  "Parse the input file to construct the initial combat state.
  Combat state is a map with keys :walls, :units, :round 0"
  [file]
  (with-open [rdr (io/reader file)]
    (reduce (fn [state [loc unit]]
              (if (= (:type unit) :wall)
                (update state :walls conj loc)
                (update state :units assoc loc (assoc unit :loc loc))))
            {:units        (sorted-map-by loc-comparator)
             :walls        (sorted-set-by loc-comparator)
             :updated      true
             :attack-power {:elf 3 :goblin 3}}
            (for [[y line] (map-indexed vector (line-seq rdr))
                  [x piece] (map-indexed vector (seq line))
                  :when (not= piece \.)]
              [[x y] (create-unit piece)]))))

(defn count-unit-type
  "Return the number of this type of unit in this combat state"
  [combat unit-type]
  (count (filter #(= (:type %) unit-type) (map second (:units combat)))))

(defn score
  "Comput score of this combat state"
  [{:keys [units rounds], :or {rounds 1}}]
  (* (dec rounds) (reduce + (map :hit-points (vals units)))))

; Neighbors
(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])
(def adjacent
  "In reading order"
  (juxt up left right down))

(defn compute-next-loc
  "Return a sequence of locations around this one in the map that are legal moves"
  [[base path] legal-fn]
  (let [new-path (cons base path)]
    (map #(vector % new-path) (filter legal-fn (adjacent base)))))

(defn available-fn
  "Return a function that given a loc returns true iff it is available to extend a path"
  [{:keys [walls units]} seen my-type]
  (fn [loc]
    (not (or (contains? walls loc)
             (contains? seen loc)
             (if-let [hit (units loc)]
               (= my-type (:type hit)))))))

(defn find-enemy
  "Find the first location in this sequence that is occupied by an enemy"
  [{units-map :units} my-type locs]
  #_{:pre  [(enter :find-enemy locs)]
     :post [(leave :find-enemy %)]}
  (some #(if (not= my-type (:type %)) %) (map units-map locs)))

(defn path-to-target
  "Find the shortest path to closest target in y,x priority.
  Return {:unit <loc> :path <seq of locs between unit and target> :target <loc>}
  or nil if no targets are reachable."
  ([combat unit-loc]
   (path-to-target combat unit-loc (:type (get (:units combat) unit-loc))))
  ([combat home-loc my-type]
   (loop [seen-previously (sorted-set-by loc-comparator)
          paths (assoc (sorted-map-by loc-comparator) home-loc nil)]
     (if-let [hit (find-enemy combat my-type (keys paths))]
       (rest (reverse (paths (:loc hit))))
       (let [seen (reduce conj seen-previously (keys paths))
             next-paths
             (reduce (fn [acc-paths path]
                       (reduce (fn [new-paths [head path]]
                                 (if-not (contains? new-paths head)
                                   (assoc new-paths head path)
                                   new-paths))
                               acc-paths
                               (compute-next-loc path (available-fn combat seen my-type))))
                     (sorted-map-by loc-comparator)
                     paths)]
         (if (not-empty next-paths)
           (recur seen next-paths)))))))

(defn move
  "Move a unit from one location to another empty location and return the updated combat state"
  [combat unit new-loc]
  #_{:pre  [(not (contains? (:units combat) new-loc))
            (enter :move "from" (:loc unit) "to" new-loc)]
     :post [(leave :move)]}
  (assoc combat
    :units
    (dissoc (assoc (:units combat)
              new-loc (assoc unit :loc new-loc))
            (:loc unit))))

(defn pick-fight
  "Choose the adjacent enemy that has the lowest hit point in reading order"
  [combat unit]
  #_{:pre  [(enter :pick-fight unit)]
     :post [(leave :pick-fight %)]}
  (let [enemy-type (enemy (:type unit))
        enemies
        (->> (adjacent (:loc unit))
             (map (:units combat))
             (filter #(= (:type %) enemy-type)))]
    (if-not (empty? enemies)
      ; Find the lowest hit points, reading order if tied
      (let [least-points (apply min (map :hit-points enemies))]
        (some #(if (= least-points (:hit-points %)) %) enemies)))))

(defn attack
  "Attack the adjacent target with the fewest points. Return new combat state with updated units after the attack"
  [combat attacker enemy]
  #_{:pre [(do (println (:type attacker) (:loc attacker) "attacks" (:type enemy) (:loc enemy)) true)]}
  (let [remaining-points (- (:hit-points enemy) (get (:attack-power combat) (:type attacker)))]
    (assoc combat
      :units
      (if (<= remaining-points 0)
        (do #_(println "Defeated! " enemy)
          (dissoc (:units combat) (:loc enemy)))
        (assoc (:units combat) (:loc enemy) (assoc enemy :hit-points remaining-points))))))

(defn fight
  "If there is an enemy adjacent, attack and return updated combat state. Else return nil."
  [combat attacker]
  (if-let [enemy (pick-fight combat attacker)]
    (attack combat attacker enemy)))

(defn tick
  "Do one round of moves/attacks. Return combat with :updated true if any moves or attacks occurred in this round"
  [{:keys [units walls rounds], :or {rounds -1} :as combat}]
  #_{:post [(do (log-round %) true)]}
  (let [round (reduce (fn [{:keys [units] :as round} unit]
                        ; Get the current state of this unit in the map. If it is there, it didn't move yet.
                        (let [next-unit (units (:loc unit))]
                          ; Check this is still occupied by this unit. Else skip.
                          (or (if (= (:id next-unit) (:id unit))
                                ; Fight if you can, else move and then fight if you can.
                                (if-let [result (fight round next-unit)]
                                  (assoc result :updated true)
                                  (if-let [next-loc (first (path-to-target round (:loc next-unit)))]
                                    (let [new-round (move round next-unit next-loc)
                                          moved-unit (get (:units new-round) next-loc)]
                                      #_(println "moved" (:type next-unit) "from" (:loc next-unit) "to" next-loc)
                                      ; Attack if you are now next to an enemy
                                      (assoc (or (fight new-round moved-unit) new-round) :updated true)))))
                              round)))
                      (assoc combat :updated false)
                      ; Get a sequence of units in reading order at the beginning of the round
                      (map second units))]
    (assoc combat
      :units (:units round)
      :updated (:updated round)
      :rounds (inc rounds))))

(defn run
  "Do rounds of combat until nobody can do anything."
  [combat]
  #_{:pre  [(enter "run" " with elf attack power of " (get-in combat [:attack-power :elf]))]
     :post [(leave "run" " goblin count: " (count-unit-type % :elf))]}
  (first (drop-while :updated (iterate tick (tick combat)))))

(defn part1 [input]
  (score (run (create-combat-state input))))

;; (part1 input-data)
;; => 229798

;; Part 2
(defn part2 [input]
  (let [combat (create-combat-state input)
        starting-elves (count-unit-type combat :elf)
        bigwin (->> combat
                    (iterate #(update-in % [:attack-power :elf] inc))
                    (map run)
                    (drop-while #(< (count-unit-type % :elf) starting-elves))
                    first)]
    {:lowest-elf-attack-power (get-in bigwin [:attack-power :elf])
     :rounds                  (:rounds bigwin)
     :score                   (score bigwin)}))

;; (part2 input-data)
;; => 52972, attack power 19, rounds 39

