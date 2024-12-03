(ns adventofcode2020.day20)

(def input-data "resources/2020/day20-test.txt")

(defn process-file [file f]
  (with-open [rdr (clojure.java.io/reader file)]
    (f (line-seq rdr))))

(defn parse-id [s] (second (re-matches #"Tile (\d+):" s)))

(defn prepare [tile]
  (as-> tile t
        (assoc t :north (first (:image t)))
        (assoc t :south (last (:image t)))
        (assoc t :east (apply str (map last (:image t))))
        (assoc t :west (apply str (map first (:image t))))))

(defn create-tile [id] {:id id :image []})

(defn build-tiles
  [lines]
  (loop [cur nil tiles nil [line & remaining] lines]
    (if (nil? line)
      (cons (prepare cur) tiles)
      (let [id (parse-id line)]
        (recur
          (cond id (create-tile id) (= line "") cur :else (update cur :image conj line))
          (if (and cur id) (cons (prepare cur) tiles) tiles)
          remaining)))))

(defn index-tile [index t]
  (as-> index m
        (update m :north update (:north t) conj t)
        (update m :south update (:south t) conj t)
        (update m :east update (:east t) conj t)
        (update m :west update (:west t) conj t)))

(defn index-tiles [tiles]
  (reduce index-tile {} tiles))

(defn place-tile
  [m t]
  (as-> t m
        (assoc m :north (get-in index [:south (:north m)]))
        (assoc m :south (get-in index [:north (:south m)]))
        (assoc m :east (get-in index [:west (:east m)]))
        (assoc m :west (get-in index [:east (:west m)]))))

(defn assemble [m [t & remaining]]
  (let []))

(defn solve [m]
  (let [placements (map (partial place-tile m) (:tiles m))]
    (reduce (fn [image tile]
              ()))))

(defn rotations
  "Create a lazy sequence of all rotations of this tile"
  [tile]
  (map #(assoc % :id (:id tile))
       (take 4 (map #(zipmap [:north :south :east :west] (vals %))
                    (partition 4 1 (cycle (select-keys tile [:north :south :east :west])))))))

(defn place-tiles [m [t & remaining]]
  (if (nil? t)
    (solve m)
    (some  #(place-tiles (index-tile m %) remaining) (rotations t))))


(defn part1 [file]
  (solve {:tiles (process-file file build-tiles)}))
