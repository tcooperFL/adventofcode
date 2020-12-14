(ns adventofcode2020.day11)

(def input-data "resources/2020/day11-input.txt")

(defn get-data
  "Get the data as an array (rows) of arrays (seats)"
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (into [] (map vec) (line-seq rdr))))

(defn location [room r c] [room r c])

(defn inspect [[room r c]] (get-in (:positions room) [r c]))

(defn occupied? [loc] (= (inspect loc) \#))

(defn move [[room r c] fx fy] [room (fx r) (fy c)])

(def directions
  (for [dx [dec identity inc] dy [dec identity inc]
        :when (not= [identity identity] [dx dy])] [dx dy]))

(defn surrounding [[room r c]]
  (map (fn [[fx fy]] (move [room r c] fx fy)) directions))

(defn occupied-adjacent [loc]
  (count (filter occupied? (surrounding loc))))

(defn tick
  "Create the next state for this position"
  [[room :as loc]]
  (condp = (inspect loc)
    \L (if ((:roomy? room) loc) \# \L)
    \# (if ((:crowded? room) loc) \L \#)
    (inspect loc)))

(defn round
  "Create the next round of the seating state"
  [{:keys [rows columns generation] :as room}]
  (assoc room
    :generation (inc generation)
    :positions
    (mapv (fn [r]
            (mapv (fn [c] (tick (location room r c)))
                  (range columns)))
          (range rows))))

(defn total-occupied
  "Count the number of occupied seats in the room"
  [room]
  (count (filter #{\#} (flatten (:positions room)))))

(defn create-room [positions]
  {:positions  positions
   :rows       (count positions)
   :columns    (count (first positions))
   :roomy?     #(zero? (occupied-adjacent %))
   :crowded?   #(>= (occupied-adjacent %) 4)
   :generation 0})

(defn solve1
  "Keep iterating on rounds until positions don't change."
  [room]
  (total-occupied
    (ffirst (drop-while (fn [[r1 r2]] (not= (:positions r1) (:positions r2)))
                        (partition 2 1 (iterate round room))))))

(defn part1 [file]
  (solve1 (create-room (get-data file))))

; (time (part1 input-data))
; "Elapsed time: 3832.046739 msecs"
; => 2344

(defn in-sight
  "What is the first non-empty item in each direction?"
  [location]
  (map (fn [[fx fy]]
         (first
           (drop-while #(= (inspect %) \.) (rest (iterate #(move % fx fy) location)))))
       directions))

(defn occupied-in-sight
  "How many occupied seats can I see in all directions at any distance?"
  [loc]
  (count (filter occupied? (in-sight loc))))

(defn part2 [file]
  (solve1 (merge (create-room (get-data file))
                 {:roomy?   #(zero? (occupied-in-sight %))
                  :crowded? #(>= (occupied-in-sight %) 5)})))

; (time (part2 input-data))
; "Elapsed time: 5456.393414 msecs"
; => 2076
